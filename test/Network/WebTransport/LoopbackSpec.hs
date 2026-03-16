-- | Loopback integration test: Haskell WebTransport client ↔ server.
--
-- Starts a local WebTransport server, connects from a client, opens
-- streams, and verifies bidirectional data exchange.
module Network.WebTransport.LoopbackSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import Test.Hspec

import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Internal as QI
import qualified Network.QUIC.Server as QS
import Network.TLS (Credentials (..), credentialLoadX509)

import Network.WebTransport.Client (ClientConfig (..), ClientSession (..), defaultClientConfig)
import qualified Network.WebTransport.Client as Client
import Network.WebTransport.Error (WebTransportError (..))
import Network.WebTransport.Internal.ExtendedConnect
import Network.WebTransport.Internal.H3Settings
import Network.WebTransport.Internal.SessionStream
import Network.WebTransport.Internal.VarInt
import Network.WebTransport.Server (ServerConfig (..), ServerSession (..))
import qualified Network.WebTransport.Server as Server
import Network.WebTransport.Stream (BidiStream (..), SessionId (..))
import qualified Network.WebTransport.Stream as Stream

spec :: Spec
spec = describe "Loopback" $ do
  -- These tests verify the full WebTransport session lifecycle:
  -- QUIC connection → H3 SETTINGS exchange → extended CONNECT → session → streams

  it "client connects to server and exchanges data on bidi stream" $ do
    withEchoServer $ \port -> do
      let cfg = testClientConfig port
      Client.connect cfg $ \session -> do
        bidi <- Client.openBi session
        let (tx, rx) = Stream.split bidi
        Stream.send tx "hello from client"
        Stream.finish tx
        received <- readAll rx
        received `shouldBe` "hello from client"

  it "client opens multiple bidi streams" $ do
    withEchoServer $ \port -> do
      let cfg = testClientConfig port
      Client.connect cfg $ \session -> do
        results <- mapM (\i -> do
          bidi <- Client.openBi session
          let (tx, rx) = Stream.split bidi
              msg = "message " <> BS.pack [i + 0x30]
          Stream.send tx msg
          Stream.finish tx
          readAll rx
          ) [1..3]
        results `shouldBe`
          [ "message 1"
          , "message 2"
          , "message 3"
          ]

  it "handles empty messages" $ do
    withEchoServer $ \port -> do
      let cfg = testClientConfig port
      Client.connect cfg $ \session -> do
        bidi <- Client.openBi session
        let (tx, rx) = Stream.split bidi
        Stream.finish tx
        received <- readAll rx
        received `shouldBe` BS.empty

-- | Read all data from a RecvStream until FIN.
readAll :: Stream.RecvStream -> IO BS.ByteString
readAll rx = go BS.empty
  where
    go acc = do
      chunk <- Stream.recv rx 4096
      if BS.null chunk
        then pure acc
        else go (acc <> chunk)

testClientConfig :: Int -> ClientConfig
testClientConfig port = defaultClientConfig
  { ccServerName = "127.0.0.1"
  , ccPort = port
  , ccPath = "/test"
  , ccOrigin = "https://localhost"
  , ccValidate = False
  }

-- | Start a loopback echo server on a random port, run an action, then stop.
withEchoServer :: (Int -> IO a) -> IO a
withEchoServer action = do
  cred <- loadTestCredentials
  -- Create a UDP socket bound to port 0 to get a random available port
  sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
  boundAddr <- NS.getSocketName sock
  let port = case boundAddr of
        NS.SockAddrInet p _ -> fromIntegral p
        _ -> error "unexpected socket address type"

  let serverCfg = QS.defaultServerConfig
        { QS.scAddresses = [("127.0.0.1", fromIntegral port)]
        , QS.scCredentials = cred
        , QI.scParameters =
            (QI.scParameters QS.defaultServerConfig)
              { QI.maxIdleTimeout = QI.Milliseconds 30000
              }
        , QS.scALPN = Just (\_ _ -> return "h3")
        }

  ready <- newEmptyMVar

  bracket
    (async $ do
      -- Use runWithSockets to reuse our pre-bound socket
      QS.runWithSockets [sock] serverCfg $ \conn -> do
        _ <- tryPutMVar ready ()
        handleServerConnection conn
    )
    (\serverThread -> do
      cancel serverThread
      NS.close sock
    )
    $ \_ -> do
      -- Wait for the server to start accepting, or timeout
      threadDelay 100000  -- 100ms for server to set up
      _ <- tryPutMVar ready ()  -- unblock if not yet
      action port

-- | Handle a QUIC connection on the server side.
--
-- Performs the H3 control stream handshake and echoes data on bidi streams.
-- This is a manual server implementation (bypassing Server.runServer)
-- so we can control the socket binding.
handleServerConnection :: QUIC.Connection -> IO ()
handleServerConnection conn = do
  -- Send control stream with SETTINGS
  ctrl <- QUIC.unidirectionalStream conn
  QUIC.sendStream ctrl (encodeVarInt h3ControlStreamType)
  let settingsPayload = encodeSettings defaultWebTransportSettings
      settingsFrame = encodeH3Frame H3FrameSettings settingsPayload
  QUIC.sendStream ctrl settingsFrame
  -- QPACK encoder/decoder streams
  qEnc <- QUIC.unidirectionalStream conn
  QUIC.sendStream qEnc (encodeVarInt qpackEncoderStreamType)
  qDec <- QUIC.unidirectionalStream conn
  QUIC.sendStream qDec (encodeVarInt qpackDecoderStreamType)

  -- Accept streams
  let loop = do
        result <- try @SomeException $ QUIC.acceptStream conn
        case result of
          Left _ -> pure ()
          Right strm -> do
            let sid = QUIC.streamId strm
            _ <- forkIO $ handleStream conn strm sid
            loop
  loop

-- | Handle a single accepted stream.
handleStream :: QUIC.Connection -> QUIC.Stream -> QUIC.StreamId -> IO ()
handleStream conn strm sid
  | QUIC.isClientInitiatedBidirectional sid = handleBidiStreamServer conn strm
  | QUIC.isClientInitiatedUnidirectional sid = handleUniStreamServer strm
  | otherwise = pure ()

-- | Handle a client bidi stream: could be HTTP/3 request or WT bidi.
handleBidiStreamServer :: QUIC.Connection -> QUIC.Stream -> IO ()
handleBidiStreamServer _conn strm = do
  -- Read initial data
  buf <- readAtLeastStream strm 1
  if BS.null buf then pure ()
  else case decodeBidiPrefix buf of
    -- WebTransport bidi stream (0x41 + session ID); echo the body
    Right (_sessId, rest) -> echoData strm rest
    Left _ ->
      -- Not a WT bidi stream; try as H3 HEADERS frame (CONNECT request)
      case decodeH3Frame buf of
        Right (H3FrameHeaders, headerBlock, _rest) ->
          case decodeHeaders headerBlock of
            Right headers -> do
              let method = lookup ":method" headers
                  proto = lookup ":protocol" headers
              case (method, proto) of
                (Just "CONNECT", Just "webtransport") -> do
                  let responseBlock = encodeConnectResponse
                      responseFrame = encodeH3Frame H3FrameHeaders responseBlock
                  QUIC.sendStream strm responseFrame
                  pure ()
                _ -> pure ()
            Left _ -> pure ()
        _ -> pure ()

-- | Echo data on a bidi stream (read → write → FIN).
echoData :: QUIC.Stream -> BS.ByteString -> IO ()
echoData strm leftover = do
  -- First send any leftover data from the prefix read
  if not (BS.null leftover) then QUIC.sendStream strm leftover else pure ()
  -- Read and echo
  let loop = do
        chunk <- QUIC.recvStream strm 4096
        if BS.null chunk
          then QUIC.shutdownStream strm
          else do
            QUIC.sendStream strm chunk
            loop
  loop

-- | Handle a client uni stream (control, QPACK, or WT uni).
handleUniStreamServer :: QUIC.Stream -> IO ()
handleUniStreamServer strm = do
  buf <- readAtLeastStream strm 1
  if BS.null buf then pure ()
  else case decodeVarInt buf of
    Right (0x00, _) -> pure ()  -- H3 control — read SETTINGS, ignore
    Right (0x02, _) -> pure ()  -- QPACK encoder — ignore
    Right (0x03, _) -> pure ()  -- QPACK decoder — ignore
    _ -> pure ()

readAtLeastStream :: QUIC.Stream -> Int -> IO BS.ByteString
readAtLeastStream strm minBytes = go BS.empty
  where
    go acc
      | BS.length acc >= minBytes = pure acc
      | otherwise = do
          chunk <- QUIC.recvStream strm 4096
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)

-- | Load test TLS credentials from PEM files.
-- Tries multiple paths since cabal test may run from either the
-- project root or the package directory.
loadTestCredentials :: IO Credentials
loadTestCredentials = tryPaths
  [ ("webtransport/test/servercert.pem", "webtransport/test/serverkey.pem")
  , ("test/servercert.pem", "test/serverkey.pem")
  ]
  where
    tryPaths [] = error "Failed to load test credentials: PEM files not found in any candidate path"
    tryPaths ((cert, key) : rest) = do
      result <- try @SomeException $ credentialLoadX509 cert key
      case result of
        Right (Right cred) -> pure (Credentials [cred])
        _ -> tryPaths rest
