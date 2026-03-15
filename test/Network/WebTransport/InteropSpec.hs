-- | Interop tests: Haskell WebTransport client ↔ webtransport-go server.
--
-- Validates RFC 9220 conformance by testing against the quic-go
-- WebTransport implementation, the most battle-tested reference
-- (used in Chrome/quic-go interop testing).
--
-- The Go echo server accepts WebTransport sessions on @/test@ and
-- echoes data on every bidi stream. This tests that our:
--
--   * H3 SETTINGS encoding is understood by quic-go
--   * QPACK-encoded extended CONNECT headers are valid
--   * Session ID framing on bidi streams is correct
--   * Stream lifecycle (send, FIN, receive) works cross-implementation
module Network.WebTransport.InteropSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , ProcessHandle
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import Test.Hspec

import Network.WebTransport.Client (ClientConfig (..), defaultClientConfig)
import qualified Network.WebTransport.Client as Client
import qualified Network.WebTransport.Stream as Stream

spec :: Spec
spec = describe "Interop (webtransport-go)" $ do
  mBinary <- runIO findGoBinary

  case mBinary of
    Nothing ->
      it "Haskell client ↔ Go server (skipped: echo server not found)" $
        pendingWith
          "webtransport-go echo server not found. Build with: cd webtransport/interop && go build -o wt-echo-server ."

    Just binPath -> do
      it "bidi stream echo roundtrip" $ do
        withGoServer binPath $ \host port -> do
          let cfg = interopClientConfig host port
          Client.connect cfg $ \session -> do
            bidi <- Client.openBi session
            let (tx, rx) = Stream.split bidi
            Stream.send tx "hello from Haskell"
            Stream.finish tx
            received <- readAll rx
            received `shouldBe` "hello from Haskell"

      it "multiple bidi streams on one session" $ do
        withGoServer binPath $ \host port -> do
          let cfg = interopClientConfig host port
          Client.connect cfg $ \session -> do
            results <- mapM (\i -> do
              bidi <- Client.openBi session
              let (tx, rx) = Stream.split bidi
                  msg = "msg-" <> BS.pack [i + 0x30]
              Stream.send tx msg
              Stream.finish tx
              readAll rx
              ) [1..5]
            results `shouldBe`
              [ "msg-1", "msg-2", "msg-3", "msg-4", "msg-5" ]

      it "large payload roundtrip" $ do
        withGoServer binPath $ \host port -> do
          let cfg = interopClientConfig host port
          Client.connect cfg $ \session -> do
            bidi <- Client.openBi session
            let (tx, rx) = Stream.split bidi
                -- 64KB payload
                payload = BS.replicate 65536 0x42
            Stream.send tx payload
            Stream.finish tx
            received <- readAll rx
            received `shouldBe` payload

      it "empty stream (FIN only)" $ do
        withGoServer binPath $ \host port -> do
          let cfg = interopClientConfig host port
          Client.connect cfg $ \session -> do
            bidi <- Client.openBi session
            let (tx, rx) = Stream.split bidi
            Stream.finish tx
            received <- readAll rx
            received `shouldBe` BS.empty

-- | Read all data until FIN.
readAll :: Stream.RecvStream -> IO BS.ByteString
readAll rx = go BS.empty
  where
    go acc = do
      chunk <- Stream.recv rx 4096
      if BS.null chunk
        then pure acc
        else go (acc <> chunk)

interopClientConfig :: String -> Int -> ClientConfig
interopClientConfig host port = defaultClientConfig
  { ccServerName = host
  , ccPort = port
  , ccPath = "/test"
  , ccOrigin = "https://localhost"
  , ccValidate = False
  }

-- | Start the Go echo server, run an action, then stop it.
withGoServer :: FilePath -> (String -> Int -> IO a) -> IO a
withGoServer binPath action = do
  withSystemTempDirectory "wt-interop" $ \tmpDir -> do
    let certPath = tmpDir </> "cert.pem"
        keyPath = tmpDir </> "key.pem"
        addrPath = tmpDir </> "addr.txt"

    (_, _, _, ph) <- createProcess (proc binPath
      [ "--addr", "127.0.0.1:0"
      , "--cert-out", certPath
      , "--key-out", keyPath
      , "--addr-out", addrPath
      , "--path", "/test"
      ])
      { std_out = CreatePipe
      , std_err = CreatePipe
      }

    -- Wait for addr file
    addrBS <- pollForFile addrPath 50000 20
    -- Extra delay for server to fully bind
    threadDelay 200000

    let addrTxt = T.strip (TE.decodeUtf8 addrBS)
        (hostPart, portPart) = T.breakOnEnd ":" addrTxt
        host = T.unpack (T.dropEnd 1 hostPart)
    port <- case reads (T.unpack portPart) of
      [(p, "")] -> pure p
      _ -> error ("failed to parse port from: " <> show addrBS)

    result <- try @IOException $ action host port

    terminateProcess ph
    _ <- waitForProcess ph

    case result of
      Right a -> pure a
      Left e -> error (show e)

pollForFile :: FilePath -> Int -> Int -> IO BS.ByteString
pollForFile path delayUs maxAttempts = go 0 delayUs
  where
    go attempt delay
      | attempt >= maxAttempts =
          error ("timed out waiting for file: " <> path)
      | otherwise = do
          result <- try @IOException $ BS.readFile path
          case result of
            Right bs | not (BS.null bs) -> pure bs
            _ -> do
              threadDelay delay
              go (attempt + 1) (min (delay * 2) 2000000)

-- | Find the Go echo server binary.
findGoBinary :: IO (Maybe FilePath)
findGoBinary = go candidates
  where
    candidates =
      [ "webtransport/interop/wt-echo-server"
      , "interop/wt-echo-server"
      ]
    go [] = pure Nothing
    go (p : ps) = do
      result <- try @IOException $ do
        (_, _, _, ph) <- createProcess (proc p ["--help"])
          { std_out = CreatePipe
          , std_err = CreatePipe
          }
        _ <- waitForProcess ph
        pure ()
      case result of
        Right _ -> pure (Just p)
        Left _ -> go ps
