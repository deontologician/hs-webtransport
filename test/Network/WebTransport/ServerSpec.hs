module Network.WebTransport.ServerSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Test.Hspec

import Network.WebTransport.Internal.H3Settings

spec :: Spec
spec = describe "Server" $ do
  describe "control stream encoding" $ do
    it "SETTINGS frame is well-formed" $ do
      let payload = encodeSettings defaultWebTransportSettings
          frame = encodeH3Frame H3FrameSettings payload
      case decodeH3Frame frame of
        Right (H3FrameSettings, decoded, _) ->
          decodeSettings decoded `shouldBe` Right defaultWebTransportSettings
        Right (other, _, _) ->
          expectationFailure ("unexpected frame type: " <> show other)
        Left err -> expectationFailure err

  describe "CLOSE_WEBTRANSPORT_SESSION capsule" $ do
    it "encodes as H3 frame type 0x2843" $ do
      let payload = encodeClosePayload 0 ""
          frame = encodeH3Frame (H3FrameUnknown 0x2843) payload
      case decodeH3Frame frame of
        Right (H3FrameUnknown 0x2843, _, _) -> pure ()
        Right (other, _, _) ->
          expectationFailure ("unexpected frame type: " <> show other)
        Left err -> expectationFailure err

-- Helper (mirrors the encoding in Server/Client modules).
encodeClosePayload :: Word64 -> BS.ByteString -> BS.ByteString
encodeClosePayload code msg =
  BS.pack
    [ fromIntegral (code `div` 0x1000000 `mod` 0x100)
    , fromIntegral (code `div` 0x10000 `mod` 0x100)
    , fromIntegral (code `div` 0x100 `mod` 0x100)
    , fromIntegral (code `mod` 0x100)
    ] <> msg
