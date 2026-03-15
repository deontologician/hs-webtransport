module Network.WebTransport.SessionStreamSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Network.WebTransport.Internal.SessionStream
import Network.WebTransport.Internal.VarInt

spec :: Spec
spec = describe "SessionStream" $ do
  describe "bidi prefix" $ do
    it "roundtrips session IDs" $ hedgehog $ do
      sid <- forAll $ Gen.word64 (Range.linear 0 1000000)
      let encoded = encodeBidiPrefix sid
      case decodeBidiPrefix encoded of
        Right (decoded, rest) -> do
          decoded === sid
          rest === BS.empty
        Left err -> do
          annotate err
          failure

    it "preserves trailing data" $ do
      let encoded = encodeBidiPrefix 42 <> "trailing"
      case decodeBidiPrefix encoded of
        Right (sid, rest) -> do
          sid `shouldBe` 42
          rest `shouldBe` "trailing"
        Left err -> expectationFailure err

  describe "uni prefix" $ do
    it "roundtrips with stream type 0x54" $ hedgehog $ do
      sid <- forAll $ Gen.word64 (Range.linear 0 1000000)
      let encoded = encodeUniPrefix sid
      case decodeUniPrefix encoded of
        Right (streamType, mSid, rest) -> do
          streamType === wtUniStreamType
          mSid === Just sid
          rest === BS.empty
        Left err -> do
          annotate err
          failure

    it "returns Nothing for non-WT stream types" $ do
      let encoded = encodeVarInt h3ControlStreamType <> "data"
      case decodeUniPrefix encoded of
        Right (streamType, mSid, rest) -> do
          streamType `shouldBe` h3ControlStreamType
          mSid `shouldBe` Nothing
          rest `shouldBe` "data"
        Left err -> expectationFailure err

  describe "stream type constants" $ do
    it "WT uni stream type is 0x54" $
      wtUniStreamType `shouldBe` 0x54

    it "H3 control stream type is 0x00" $
      h3ControlStreamType `shouldBe` 0x00

    it "QPACK encoder stream type is 0x02" $
      qpackEncoderStreamType `shouldBe` 0x02

    it "QPACK decoder stream type is 0x03" $
      qpackDecoderStreamType `shouldBe` 0x03
