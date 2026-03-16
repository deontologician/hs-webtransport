-- | WebTransport client API.
--
-- Connects to a WebTransport server via QUIC + HTTP/3, performs
-- the SETTINGS exchange, and establishes sessions via extended CONNECT.
--
-- For a single session:
--
-- @
-- connect defaultClientConfig { ccServerName = "localhost", ccPort = 4433 } $ \\session -> do
--   bidi <- openBi session
--   let (tx, rx) = Stream.split bidi
--   Stream.send tx "hello"
--   Stream.finish tx
--   msg <- Stream.recv rx 1024
--   print msg
-- @
--
-- For multiple sessions on one connection:
--
-- @
-- withConnection cfg $ \\cx -> do
--   session1 <- newSession cx "\/path1"
--   session2 <- newSession cx "\/path2"
--   -- use sessions independently...
-- @
module Network.WebTransport.Client
  ( -- * Types
    ClientSession (..)
  , ClientConfig (..)
  , ClientConnection (..)
  , defaultClientConfig
    -- * Connecting
  , connect
  , withConnection
  , newSession
    -- * Session operations
  , openBi
  , openUni
  , acceptBi
  , acceptUni
  , closeSession
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QC
import qualified Network.QUIC.Internal as QI
import Network.TLS.QUIC (ExtensionRaw (..), ExtensionID (..))


import Network.WebTransport.Error (WebTransportError (..))
import Network.WebTransport.Internal.ExtendedConnect
import Network.WebTransport.Internal.H3Settings
import Network.WebTransport.Internal.SessionStream
import Network.WebTransport.Internal.VarInt
import Network.WebTransport.Stream (BidiStream (..), RecvStream (..), SendStream (..), SessionId (..))

-- | A WebTransport session from the client's perspective.
data ClientSession = ClientSession
  { csSessionId     :: !SessionId
  , csConnection    :: !QUIC.Connection
  , csConnectStrm   :: !QUIC.Stream
  , csIncoming      :: !(TQueue IncomingStream)
  , csClosed        :: !(TVar Bool)
  }

-- | A WebTransport-capable HTTP/3 connection that can host multiple sessions.
--
-- Create with 'withConnection', then use 'newSession' to establish
-- WebTransport sessions on it.
data ClientConnection = ClientConnection
  { cxConnection     :: !QUIC.Connection
  , cxSessions       :: !(TVar (Map.Map Word64 ClientSession))
  , cxPeerSettingsOk :: !(IORef Bool)
  , cxConfig         :: !ClientConfig
  }

data IncomingStream
  = IncomingBidi !QUIC.Stream
  | IncomingUni  !QUIC.Stream

-- | Client configuration.
data ClientConfig = ClientConfig
  { ccServerName :: !String
  , ccPort       :: !Int
  , ccPath       :: !BS.ByteString       -- ^ e.g. @"/wt"@
  , ccOrigin     :: !BS.ByteString       -- ^ e.g. @"https://example.com"@
  , ccValidate   :: !Bool                -- ^ TLS certificate validation
  }

-- | Default client config. You must set 'ccServerName' and 'ccPort'.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { ccServerName = "localhost"
  , ccPort       = 4433
  , ccPath       = "/"
  , ccOrigin     = "https://localhost"
  , ccValidate   = False
  }

-- | Establish a WebTransport-capable HTTP/3 connection.
--
-- Opens a QUIC connection with ALPN @h3@, sends the HTTP/3 control stream
-- with SETTINGS, and starts the connection-level stream demuxer.
-- Multiple sessions can be created on this connection using 'newSession'.
withConnection :: ClientConfig -> (ClientConnection -> IO a) -> IO a
withConnection cfg callback = do
  let qcfg = QC.defaultClientConfig
        { QC.ccServerName = ccServerName cfg
        , QC.ccPortName = show (ccPort cfg)
        , QC.ccALPN = \_ -> return (Just ["h3"])
        , QC.ccValidate = ccValidate cfg
        , QI.ccParameters =
            (QI.ccParameters QC.defaultClientConfig)
              { QI.maxIdleTimeout = QI.Milliseconds 30000
              }
        , QI.ccHooks =
            (QI.ccHooks QC.defaultClientConfig)
              { QI.onTLSExtensionCreated = injectDatagramParam
              }
        }
  QC.run qcfg $ \conn -> do
    -- 1. Open control streams and send SETTINGS
    sendControlStream conn

    -- 2. Set up connection state
    peerSettingsOk <- newIORef False
    sessions <- newTVarIO Map.empty
    let cx = ClientConnection
          { cxConnection     = conn
          , cxSessions       = sessions
          , cxPeerSettingsOk = peerSettingsOk
          , cxConfig         = cfg
          }

    -- 3. Start connection-level demuxer
    _ <- forkIO $ connectionDemuxer cx

    callback cx

-- | Create a new WebTransport session on an existing connection.
--
-- Sends an extended CONNECT request with the given path and waits for
-- a 2xx response from the server.  The CONNECT stream ID becomes the
-- session ID.
newSession :: ClientConnection -> BS.ByteString -> IO ClientSession
newSession cx path = do
  let cfg = cxConfig cx
      conn = cxConnection cx

  -- Send extended CONNECT
  connectStrm <- QUIC.stream conn
  let authority = BS.pack (map (fromIntegral . fromEnum) (ccServerName cfg))
                    <> ":" <> BS.pack (map (fromIntegral . fromEnum) (show (ccPort cfg)))
      requestBlock = encodeConnectRequest authority path (ccOrigin cfg)
      requestFrame = encodeH3Frame H3FrameHeaders requestBlock
  QUIC.sendStream connectStrm requestFrame

  -- Read response
  responseBuf <- readAtLeast connectStrm 1
  case decodeH3Frame responseBuf of
    Right (H3FrameHeaders, headerBlock, _rest) ->
      case decodeHeaders headerBlock of
        Right headers ->
          case lookup ":status" headers of
            Just "200" -> do
              -- Session established
              let sid = SessionId (fromIntegral (QUIC.streamId connectStrm))
              incoming <- newTQueueIO
              closed <- newTVarIO False
              let session = ClientSession
                    { csSessionId = sid
                    , csConnection = conn
                    , csConnectStrm = connectStrm
                    , csIncoming = incoming
                    , csClosed = closed
                    }
              -- Register in connection's session map
              atomically $ modifyTVar' (cxSessions cx) $
                Map.insert (unSessionId sid) session
              pure session
            Just status ->
              throwIO $ SessionRejected 0 status
            Nothing ->
              throwIO $ ProtocolError "missing :status in CONNECT response"
        Left err ->
          throwIO $ ProtocolError (T.pack ("failed to decode CONNECT response headers: " <> err))
    Right (other, _, _) ->
      throwIO $ ProtocolError (T.pack ("expected HEADERS frame, got: " <> show other))
    Left err ->
      throwIO $ ProtocolError (T.pack ("failed to decode response frame: " <> err))

-- | Connect to a WebTransport server and establish a single session.
--
-- This is a convenience wrapper around 'withConnection' and 'newSession'.
-- Uses bracket pattern: the session is closed when the callback returns.
connect :: ClientConfig -> (ClientSession -> IO a) -> IO a
connect cfg callback =
  withConnection cfg $ \cx -> do
    session <- newSession cx (ccPath cfg)
    callback session

-- | Send the HTTP/3 control stream with SETTINGS.
sendControlStream :: QUIC.Connection -> IO ()
sendControlStream conn = do
  ctrl <- QUIC.unidirectionalStream conn
  QUIC.sendStream ctrl (encodeVarInt h3ControlStreamType)
  let settingsPayload = encodeSettings defaultWebTransportSettings
      settingsFrame = encodeH3Frame H3FrameSettings settingsPayload
  QUIC.sendStream ctrl settingsFrame
  -- QPACK encoder/decoder streams (empty, static only)
  qEnc <- QUIC.unidirectionalStream conn
  QUIC.sendStream qEnc (encodeVarInt qpackEncoderStreamType)
  qDec <- QUIC.unidirectionalStream conn
  QUIC.sendStream qDec (encodeVarInt qpackDecoderStreamType)

-- | Connection-level demuxer for all incoming streams.
--
-- Routes H3 control\/QPACK streams at the connection level, and
-- WebTransport data streams to the appropriate session based on
-- the session ID in the stream prefix.
connectionDemuxer :: ClientConnection -> IO ()
connectionDemuxer cx = go
  where
    conn = cxConnection cx
    go = (do
      strm <- QUIC.acceptStream conn
      let sid = QUIC.streamId strm
      _ <- forkIO $
        if QUIC.isServerInitiatedUnidirectional sid
          then handleServerUni strm
          else if QUIC.isServerInitiatedBidirectional sid
            then handleServerBidi strm
            else pure ()
      go) `catch` \(_ :: SomeException) -> pure ()

    -- Handle a server-initiated unidirectional stream.
    -- Could be H3 control (0x00), QPACK (0x02/0x03), or WT uni (0x54).
    handleServerUni strm = do
      buf <- QUIC.recvStream strm 1
      if BS.null buf then pure ()
      else do
        streamType <- readFullVarInt strm buf
        case streamType of
          0x00 -> do
            -- H3 control stream — read SETTINGS
            frameBuf <- readAtLeast strm 1
            case decodeH3Frame frameBuf of
              Right (H3FrameSettings, payload, _) ->
                case decodeSettings payload of
                  Right settings ->
                    let hasWT = any (\(k, v) -> k == settingsEnableWebTransport && v == 1) settings
                     in writeIORef (cxPeerSettingsOk cx) hasWT
                  Left _ -> pure ()
              _ -> pure ()
          0x02 -> pure ()  -- QPACK encoder
          0x03 -> pure ()  -- QPACK decoder
          0x54 -> do
            -- WebTransport unidirectional stream — read session ID
            sessIdBuf <- QUIC.recvStream strm 8
            case decodeVarInt sessIdBuf of
              Right (sessIdVal, _) -> routeToSession sessIdVal (IncomingUni strm)
              Left _ -> QUIC.stopStream strm (QUIC.ApplicationProtocolError 0)
          _ -> pure ()     -- Unknown stream type — ignore per spec

    -- Handle a server-initiated bidirectional stream.
    -- Format: 0x41 varint + session ID varint + stream body
    handleServerBidi strm = do
      buf <- QUIC.recvStream strm 16
      if BS.null buf then pure ()
      else case decodeBidiPrefix buf of
        Right (sessIdVal, _leftover) ->
          routeToSession sessIdVal (IncomingBidi strm)
        Left _ -> QUIC.resetStream strm (QUIC.ApplicationProtocolError 0)

    routeToSession sessIdVal incoming = do
      sessions <- readTVarIO (cxSessions cx)
      case Map.lookup sessIdVal sessions of
        Just sess -> atomically $ writeTQueue (csIncoming sess) incoming
        Nothing -> case incoming of
          IncomingBidi s -> QUIC.resetStream s (QUIC.ApplicationProtocolError 0)
          IncomingUni s -> QUIC.stopStream s (QUIC.ApplicationProtocolError 0)

-- | Read a full QUIC varint, fetching more bytes if needed.
readFullVarInt :: QUIC.Stream -> BS.ByteString -> IO Word64
readFullVarInt strm buf = case decodeVarInt buf of
  Right (val, _) -> pure val
  Left _ -> do
    more <- QUIC.recvStream strm 8
    if BS.null more
      then throwIO $ ProtocolError "incomplete stream type varint"
      else readFullVarInt strm (buf <> more)

-- | Read at least @n@ bytes from a QUIC stream.
readAtLeast :: QUIC.Stream -> Int -> IO BS.ByteString
readAtLeast strm minBytes = go BS.empty
  where
    go acc
      | BS.length acc >= minBytes = pure acc
      | otherwise = do
          chunk <- QUIC.recvStream strm 4096
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)

-- | Open a client-initiated bidirectional stream.
--
-- Sends the WebTransport bidi stream header: frame type 0x41 followed
-- by the session ID varint. This is how HTTP/3 implementations (via
-- the StreamHijacker) identify WebTransport bidi streams.
openBi :: ClientSession -> IO BidiStream
openBi session = do
  strm <- QUIC.stream (csConnection session)
  QUIC.sendStream strm
    (encodeVarInt wtBidiFrameType <> encodeVarInt (unSessionId (csSessionId session)))
  pure $ BidiStream strm (csSessionId session)

-- | Open a client-initiated unidirectional stream.
openUni :: ClientSession -> IO SendStream
openUni session = do
  strm <- QUIC.unidirectionalStream (csConnection session)
  QUIC.sendStream strm (encodeUniPrefix (unSessionId (csSessionId session)))
  pure $ SendStream strm (csSessionId session)

-- | Accept a server-initiated bidirectional stream.
acceptBi :: ClientSession -> IO BidiStream
acceptBi session = do
  item <- atomically $ do
    closed <- readTVar (csClosed session)
    if closed
      then error "acceptBi: session closed"
      else do
        item <- readTQueue (csIncoming session)
        case item of
          IncomingBidi _ -> pure item
          IncomingUni _ -> do
            unGetTQueue (csIncoming session) item
            retry
  case item of
    IncomingBidi s -> pure $ BidiStream s (csSessionId session)
    IncomingUni _ -> error "impossible"

-- | Accept a server-initiated unidirectional stream.
acceptUni :: ClientSession -> IO RecvStream
acceptUni session = do
  item <- atomically $ do
    closed <- readTVar (csClosed session)
    if closed
      then error "acceptUni: session closed"
      else do
        item <- readTQueue (csIncoming session)
        case item of
          IncomingUni _ -> pure item
          IncomingBidi _ -> do
            unGetTQueue (csIncoming session) item
            retry
  case item of
    IncomingUni s -> pure $ RecvStream s (csSessionId session)
    IncomingBidi _ -> error "impossible"

-- | Close the session.
closeSession :: ClientSession -> Word64 -> BS.ByteString -> IO ()
closeSession session code msg = do
  atomically $ writeTVar (csClosed session) True
  let capsulePayload = encodeClosePayload code msg
      capsule = encodeH3Frame (H3FrameUnknown 0x2843) capsulePayload
  QUIC.sendStream (csConnectStrm session) capsule
  QUIC.shutdownStream (csConnectStrm session)

encodeClosePayload :: Word64 -> BS.ByteString -> BS.ByteString
encodeClosePayload code msg =
  BS.pack
    [ fromIntegral (code `div` 0x1000000 `mod` 0x100)
    , fromIntegral (code `div` 0x10000 `mod` 0x100)
    , fromIntegral (code `div` 0x100 `mod` 0x100)
    , fromIntegral (code `mod` 0x100)
    ] <> msg

-- | Inject max_datagram_frame_size (0x20) into QUIC transport parameters.
--
-- The quic package doesn't support RFC 9221 datagrams, but WebTransport
-- peers require seeing this transport parameter to enable the protocol.
-- We append it to the encoded parameters in the TLS extension.
injectDatagramParam :: [ExtensionRaw] -> [ExtensionRaw]
injectDatagramParam = map patchExt
  where
    -- Extension IDs for QUIC transport parameters
    quicTPExtId1 = 0x0039  -- QUIC v1
    quicTPExtId2 = 0xffa5  -- draft versions

    patchExt (ExtensionRaw eid bs)
      | eid == ExtensionID quicTPExtId1 || eid == ExtensionID quicTPExtId2 =
          ExtensionRaw eid (bs <> datagramParam)
      | otherwise = ExtensionRaw eid bs

    -- max_datagram_frame_size = 65535
    -- Encoded as: key varint (0x20) ++ length varint ++ value varint
    -- The value is a varint, and the length field gives the byte length
    -- of the encoded value.
    datagramParam =
      let value = encodeVarInt 65535
       in encodeVarInt 0x20                          -- parameter key
            <> encodeVarInt (fromIntegral (BS.length value))  -- value length
            <> value                                  -- max datagram frame size
