module Network.WebTransport.VarIntSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Network.WebTransport.Internal.VarInt

spec :: Spec
spec = describe "VarInt" $ do
  describe "roundtrip" $ do
    it "roundtrips arbitrary values" $ hedgehog $ do
      -- Values up to 2^62 - 1
      n <- forAll $ Gen.word64 (Range.linear 0 4611686018427387903)
      let encoded = encodeVarInt n
      case decodeVarInt encoded of
        Right (decoded, rest) -> do
          decoded === n
          rest === BS.empty
        Left err -> do
          annotate err
          failure

  describe "encoding size" $ do
    it "1-byte for 0..63" $ hedgehog $ do
      n <- forAll $ Gen.word64 (Range.linear 0 63)
      BS.length (encodeVarInt n) === 1

    it "2-byte for 64..16383" $ hedgehog $ do
      n <- forAll $ Gen.word64 (Range.linear 64 16383)
      BS.length (encodeVarInt n) === 2

    it "4-byte for 16384..1073741823" $ hedgehog $ do
      n <- forAll $ Gen.word64 (Range.linear 16384 1073741823)
      BS.length (encodeVarInt n) === 4

    it "8-byte for 1073741824..2^62-1" $ hedgehog $ do
      n <- forAll $ Gen.word64 (Range.linear 1073741824 4611686018427387903)
      BS.length (encodeVarInt n) === 8

  describe "varIntSize" $ do
    it "matches actual encoding size" $ hedgehog $ do
      n <- forAll $ Gen.word64 (Range.linear 0 4611686018427387903)
      varIntSize n === BS.length (encodeVarInt n)

  describe "RFC 9000 §A.1 known vectors" $ do
    it "encodes 37 as 0x25" $
      encodeVarInt 37 `shouldBe` BS.pack [0x25]

    it "encodes 15293 as 0x7bbd" $
      encodeVarInt 15293 `shouldBe` BS.pack [0x7b, 0xbd]

    it "encodes 494878333 as 0x9d7f3e7d" $
      encodeVarInt 494878333 `shouldBe` BS.pack [0x9d, 0x7f, 0x3e, 0x7d]

    it "encodes 151288809941952652 as 0xc2197c5eff14e88c" $
      encodeVarInt 151288809941952652 `shouldBe`
        BS.pack [0xc2, 0x19, 0x7c, 0x5e, 0xff, 0x14, 0xe8, 0x8c]

  describe "decodeVarInt with trailing data" $ do
    it "preserves remaining bytes" $ do
      let bs = encodeVarInt 42 <> BS.pack [0xDE, 0xAD]
      case decodeVarInt bs of
        Right (val, rest) -> do
          val `shouldBe` 42
          rest `shouldBe` BS.pack [0xDE, 0xAD]
        Left err -> expectationFailure err

  describe "error cases" $ do
    it "fails on empty input" $
      decodeVarInt BS.empty `shouldBe` Left "decodeVarInt: empty input"

    it "fails on truncated 2-byte encoding" $ do
      let bs = BS.pack [0x40]  -- 2-byte prefix but only 1 byte
      case decodeVarInt bs of
        Left _ -> pure ()
        Right _ -> expectationFailure "should have failed"
