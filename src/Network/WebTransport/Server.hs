-- | WebTransport server API.
--
-- Accepts QUIC connections, performs the HTTP/3 handshake with
-- WebTransport-enabled SETTINGS, and dispatches incoming extended
-- CONNECT requests as WebTransport sessions.
--
-- Usage:
--
-- @
-- runServer config $ \\session -> do
--   bidi <- acceptBi session
--   let (tx, rx) = Stream.split bidi
--   msg <- Stream.recv rx 1024
--   Stream.send tx msg
--   Stream.finish tx
-- @
module Network.WebTransport.Server
  ( -- * Types
    ServerSession (..)
  , ServerConfig (..)
    -- * Running
  , runServer
    -- * Session operations
  , acceptBi
  , acceptUni
  , openBi
  , openUni
  , closeSession
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Server as QS

import Network.WebTransport.Error (WebTransportError (..))
import Network.WebTransport.Internal.ExtendedConnect
import Network.WebTransport.Internal.H3Settings
import Network.WebTransport.Internal.SessionStream
import Network.WebTransport.Internal.VarInt
import Network.WebTransport.Stream (BidiStream (..), RecvStream (..), SendStream (..), SessionId (..))

-- | A WebTransport session from the server's perspective.
data ServerSession = ServerSession
  { sessId          :: !SessionId
  , sessConnection  :: !QUIC.Connection
  , sessConnectStrm :: !QUIC.Stream           -- ^ The CONNECT request stream
  , sessPath        :: !BS.ByteString
  , sessIncoming    :: !(TQueue IncomingStream) -- ^ Incoming WT streams
  , sessClosed      :: !(TVar Bool)
  }

data IncomingStream
  = IncomingBidi !QUIC.Stream
  | IncomingUni  !QUIC.Stream

-- | Server configuration.
data ServerConfig = ServerConfig
  { scQuic        :: !QS.ServerConfig
  , scAcceptPath  :: !(BS.ByteString -> Bool)  -- ^ Filter sessions by path
  }

-- | Run a WebTransport server.
--
-- Listens for QUIC connections with ALPN @h3@, performs the HTTP/3
-- control stream handshake, and calls the handler for each new
-- WebTransport session.
runServer :: ServerConfig -> (ServerSession -> IO ()) -> IO ()
runServer cfg handler = do
  let qcfg = (scQuic cfg)
        { QS.scALPN = Just (\_ _offered -> return "h3")
        }
  QS.run qcfg $ \conn -> do
    -- Set up HTTP/3 control streams (send SETTINGS) and
    -- start demuxer for incoming streams.
    handleConnection cfg conn handler
      `catch` \(_ :: SomeException) -> pure ()

-- | Handle a single QUIC connection.
handleConnection
  :: ServerConfig -> QUIC.Connection -> (ServerSession -> IO ()) -> IO ()
handleConnection cfg conn handler = do
  -- Send our control stream with SETTINGS
  sendControlStream conn

  -- State: track whether we've received peer settings
  peerSettingsOk <- newIORef False
  -- Sessions indexed by session ID (CONNECT stream ID)
  sessionsRef <- newIORef ([] :: [ServerSession])

  -- Demux loop: accept streams and route them
  let loop = do
        strm <- QUIC.acceptStream conn
        let sid = QUIC.streamId strm
        if QUIC.isClientInitiatedUnidirectional sid
          then do
            -- Read stream type
            handleUniStream conn strm peerSettingsOk sessionsRef
            loop
          else if QUIC.isClientInitiatedBidirectional sid
            then do
              -- Could be a CONNECT request or a WT bidi stream
              handleBidiStream cfg conn strm peerSettingsOk sessionsRef handler
              loop
            else
              loop

  loop

-- | Open and send the HTTP/3 control stream with our SETTINGS.
sendControlStream :: QUIC.Connection -> IO ()
sendControlStream conn = do
  ctrl <- QUIC.unidirectionalStream conn
  -- Stream type: H3 control (0x00)
  QUIC.sendStream ctrl (encodeVarInt h3ControlStreamType)
  -- SETTINGS frame
  let settingsPayload = encodeSettings defaultWebTransportSettings
      settingsFrame = encodeH3Frame H3FrameSettings settingsPayload
  QUIC.sendStream ctrl settingsFrame
  -- Also open QPACK encoder/decoder streams (empty, since we use static only)
  qEnc <- QUIC.unidirectionalStream conn
  QUIC.sendStream qEnc (encodeVarInt qpackEncoderStreamType)
  qDec <- QUIC.unidirectionalStream conn
  QUIC.sendStream qDec (encodeVarInt qpackDecoderStreamType)

-- | Handle an incoming client-initiated unidirectional stream.
handleUniStream
  :: QUIC.Connection
  -> QUIC.Stream
  -> IORef Bool
  -> IORef [ServerSession]
  -> IO ()
handleUniStream _conn strm peerSettingsOk sessionsRef = do
  _ <- forkIO $ do
    -- Read stream type varint
    firstByte <- QUIC.recvStream strm 1
    if BS.null firstByte
      then pure ()
      else do
        -- May need more bytes for varint
        let readFullVarInt buf = case decodeVarInt buf of
              Right (val, _rest) -> pure val
              Left _ -> do
                more <- QUIC.recvStream strm 8
                if BS.null more
                  then throwIO $ ProtocolError "incomplete stream type varint"
                  else readFullVarInt (buf <> more)
        streamType <- readFullVarInt firstByte
        case streamType of
          0x00 -> do
            -- H3 control stream — read SETTINGS
            parseControlStreamFrames strm peerSettingsOk
          0x02 -> pure ()  -- QPACK encoder stream — ignore (static only)
          0x03 -> pure ()  -- QPACK decoder stream — ignore (static only)
          0x54 -> do
            -- WebTransport unidirectional stream
            -- Read session ID
            sessIdBuf <- QUIC.recvStream strm 8
            case decodeVarInt sessIdBuf of
              Right (sessIdVal, _leftover) -> do
                sessions <- readIORef sessionsRef
                case filter (\s -> unSessionId (sessId s) == sessIdVal) sessions of
                  (sess:_) -> atomically $
                    writeTQueue (sessIncoming sess) (IncomingUni strm)
                  [] -> QUIC.stopStream strm (QUIC.ApplicationProtocolError 0)
              Left _ ->
                QUIC.stopStream strm (QUIC.ApplicationProtocolError 0)
          _ -> pure ()  -- Unknown stream type — ignore per spec
  pure ()

-- | Parse frames on the H3 control stream (looking for SETTINGS).
parseControlStreamFrames :: QUIC.Stream -> IORef Bool -> IO ()
parseControlStreamFrames strm peerSettingsOk = do
  -- Read enough for at least one frame
  buf <- readAtLeast strm 1
  if BS.null buf
    then pure ()
    else case decodeH3Frame buf of
      Right (H3FrameSettings, payload, _rest) -> do
        case decodeSettings payload of
          Right settings ->
            let hasWT = any (\(k, v) -> k == settingsEnableWebTransport && v == 1) settings
             in writeIORef peerSettingsOk hasWT
          Left _ -> writeIORef peerSettingsOk False
      Right _ -> pure ()  -- Other frames on control stream
      Left _ -> pure ()

-- | Read at least @n@ bytes from a QUIC stream, accumulating.
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

-- | Handle an incoming client-initiated bidirectional stream.
--
-- This could be an HTTP/3 request (extended CONNECT) or a WebTransport
-- bidirectional stream (prefixed with session ID varint).
handleBidiStream
  :: ServerConfig
  -> QUIC.Connection
  -> QUIC.Stream
  -> IORef Bool
  -> IORef [ServerSession]
  -> (ServerSession -> IO ())
  -> IO ()
handleBidiStream cfg conn strm _peerSettingsOk sessionsRef handler = do
  _ <- forkIO $ do
    -- Read initial data to determine what this stream is.
    -- For an HTTP/3 request, we'll see an H3 HEADERS frame.
    -- For a WT bidi stream, we'll see a session ID varint.
    buf <- readAtLeast strm 1
    if BS.null buf
      then pure ()
      else case decodeH3Frame buf of
        Right (H3FrameHeaders, headerBlock, _rest) ->
          -- This is an HTTP/3 request — check if it's an extended CONNECT
          case decodeHeaders headerBlock of
            Right headers -> do
              let method = lookup ":method" headers
                  proto = lookup ":protocol" headers
                  path = lookup ":path" headers
              case (method, proto, path) of
                (Just "CONNECT", Just "webtransport", Just p)
                  | scAcceptPath cfg p -> do
                      -- Accept the session
                      let responseBlock = encodeConnectResponse
                          responseFrame = encodeH3Frame H3FrameHeaders responseBlock
                      QUIC.sendStream strm responseFrame

                      let sid = SessionId (fromIntegral (QUIC.streamId strm))
                      incoming <- newTQueueIO
                      closed <- newTVarIO False
                      let session = ServerSession
                            { sessId = sid
                            , sessConnection = conn
                            , sessConnectStrm = strm
                            , sessPath = p
                            , sessIncoming = incoming
                            , sessClosed = closed
                            }
                      atomicModifyIORef' sessionsRef (\ss -> (session : ss, ()))
                      handler session
                        `catch` \(_ :: SomeException) -> pure ()
                  | otherwise ->
                      -- Path not accepted — send 404
                      pure ()
                _ ->
                  -- Not an extended CONNECT — ignore
                  pure ()
            Left _ -> pure ()
        Right _ -> pure ()
        Left _ -> do
          -- Not an H3 frame — try as WT bidi stream (session ID prefix)
          case decodeVarInt buf of
            Right (sessIdVal, _leftover) -> do
              sessions <- readIORef sessionsRef
              case filter (\s -> unSessionId (sessId s) == sessIdVal) sessions of
                (sess:_) -> atomically $
                  writeTQueue (sessIncoming sess) (IncomingBidi strm)
                [] -> QUIC.resetStream strm (QUIC.ApplicationProtocolError 0)
            Left _ ->
              QUIC.resetStream strm (QUIC.ApplicationProtocolError 0)
  pure ()

-- | Accept an incoming bidirectional stream within a session.
acceptBi :: ServerSession -> IO BidiStream
acceptBi sess = do
  item <- atomically $ do
    closed <- readTVar (sessClosed sess)
    if closed
      then error "acceptBi: session closed"
      else do
        item <- readTQueue (sessIncoming sess)
        case item of
          IncomingBidi _ -> pure item
          IncomingUni _ -> do
            -- Put it back and retry — we want bidi
            unGetTQueue (sessIncoming sess) item
            retry
  case item of
    IncomingBidi s -> pure $ BidiStream s (sessId sess)
    IncomingUni _ -> error "impossible"

-- | Accept an incoming unidirectional stream within a session.
acceptUni :: ServerSession -> IO RecvStream
acceptUni sess = do
  item <- atomically $ do
    closed <- readTVar (sessClosed sess)
    if closed
      then error "acceptUni: session closed"
      else do
        item <- readTQueue (sessIncoming sess)
        case item of
          IncomingUni _ -> pure item
          IncomingBidi _ -> do
            unGetTQueue (sessIncoming sess) item
            retry
  case item of
    IncomingUni s -> pure $ RecvStream s (sessId sess)
    IncomingBidi _ -> error "impossible"

-- | Open a server-initiated bidirectional stream in this session.
openBi :: ServerSession -> IO BidiStream
openBi sess = do
  strm <- QUIC.stream (sessConnection sess)
  -- Write session ID prefix
  QUIC.sendStream strm (encodeBidiPrefix (unSessionId (sessId sess)))
  pure $ BidiStream strm (sessId sess)

-- | Open a server-initiated unidirectional stream in this session.
openUni :: ServerSession -> IO SendStream
openUni sess = do
  strm <- QUIC.unidirectionalStream (sessConnection sess)
  -- Write stream type 0x54 + session ID prefix
  QUIC.sendStream strm (encodeUniPrefix (unSessionId (sessId sess)))
  pure $ SendStream strm (sessId sess)

-- | Close the session with an error code and message.
--
-- Sends a CLOSE_WEBTRANSPORT_SESSION capsule on the CONNECT stream.
closeSession :: ServerSession -> Word64 -> BS.ByteString -> IO ()
closeSession sess code msg = do
  atomically $ writeTVar (sessClosed sess) True
  -- Encode CLOSE_WEBTRANSPORT_SESSION capsule:
  -- 4-byte error code + UTF-8 message
  let capsulePayload = encodeClosePayload code msg
      capsule = encodeH3Frame (H3FrameUnknown 0x2843) capsulePayload
  QUIC.sendStream (sessConnectStrm sess) capsule
  QUIC.shutdownStream (sessConnectStrm sess)

-- | Encode the CLOSE_WEBTRANSPORT_SESSION capsule payload:
-- 4-byte big-endian error code followed by UTF-8 error message.
encodeClosePayload :: Word64 -> BS.ByteString -> BS.ByteString
encodeClosePayload code msg =
  BS.pack
    [ fromIntegral (code `div` 0x1000000 `mod` 0x100)
    , fromIntegral (code `div` 0x10000 `mod` 0x100)
    , fromIntegral (code `div` 0x100 `mod` 0x100)
    , fromIntegral (code `mod` 0x100)
    ] <> msg
