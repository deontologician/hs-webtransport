module Network.WebTransport.H3SettingsSpec (spec) where

import qualified Data.ByteString as BS
import Test.Hspec

import Network.WebTransport.Internal.H3Settings
import Network.WebTransport.Internal.VarInt

spec :: Spec
spec = describe "H3Settings" $ do
  describe "encodeSettings / decodeSettings roundtrip" $ do
    it "roundtrips default WebTransport settings" $ do
      let encoded = encodeSettings defaultWebTransportSettings
      case decodeSettings encoded of
        Right decoded -> decoded `shouldBe` defaultWebTransportSettings
        Left err -> expectationFailure err

    it "roundtrips empty settings" $ do
      let encoded = encodeSettings []
      decodeSettings encoded `shouldBe` Right []

    it "roundtrips single setting" $ do
      let settings = [(settingsEnableWebTransport, 1)]
          encoded = encodeSettings settings
      decodeSettings encoded `shouldBe` Right settings

  describe "encodeH3Frame / decodeH3Frame roundtrip" $ do
    it "roundtrips a SETTINGS frame" $ do
      let payload = encodeSettings defaultWebTransportSettings
          frame = encodeH3Frame H3FrameSettings payload
      case decodeH3Frame frame of
        Right (typ, pay, rest) -> do
          typ `shouldBe` H3FrameSettings
          pay `shouldBe` payload
          rest `shouldBe` BS.empty
        Left err -> expectationFailure err

    it "roundtrips a DATA frame" $ do
      let payload = "hello world"
          frame = encodeH3Frame H3FrameData payload
      case decodeH3Frame frame of
        Right (typ, pay, rest) -> do
          typ `shouldBe` H3FrameData
          pay `shouldBe` payload
          rest `shouldBe` BS.empty
        Left err -> expectationFailure err

    it "roundtrips a HEADERS frame with trailing data" $ do
      let payload = "some-headers"
          frame = encodeH3Frame H3FrameHeaders payload <> "trailing"
      case decodeH3Frame frame of
        Right (typ, pay, rest) -> do
          typ `shouldBe` H3FrameHeaders
          pay `shouldBe` payload
          rest `shouldBe` "trailing"
        Left err -> expectationFailure err

  describe "WebTransport settings keys" $ do
    it "SETTINGS_ENABLE_WEBTRANSPORT is 0x2b603742" $
      unSettingsKey settingsEnableWebTransport `shouldBe` 0x2b603742

    it "SETTINGS_H3_DATAGRAM is 0x33 (RFC 9297)" $
      unSettingsKey settingsH3Datagram `shouldBe` 0x33

    it "SETTINGS_ENABLE_CONNECT_PROTOCOL is 0x08" $
      unSettingsKey settingsEnableConnectProtocol `shouldBe` 0x08
