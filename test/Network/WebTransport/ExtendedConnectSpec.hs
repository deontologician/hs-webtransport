module Network.WebTransport.ExtendedConnectSpec (spec) where

import qualified Data.ByteString as BS
import Test.Hspec

import Network.WebTransport.Internal.ExtendedConnect

spec :: Spec
spec = describe "ExtendedConnect" $ do
  describe "encodeHeaderBlock / decodeHeaderBlock" $ do
    it "roundtrips a CONNECT request" $ do
      let block = encodeConnectRequest "localhost:4433" "/webtransport" "https://localhost"
      case decodeHeaderBlock block of
        Right headers -> do
          lookup ":method" headers `shouldBe` Just "CONNECT"
          lookup ":scheme" headers `shouldBe` Just "https"
          lookup ":authority" headers `shouldBe` Just "localhost:4433"
          lookup ":path" headers `shouldBe` Just "/webtransport"
          -- :protocol and origin are encoded as literals
          lookup ":protocol" headers `shouldBe` Just "webtransport"
          lookup "origin" headers `shouldBe` Just "https://localhost"
        Left err -> expectationFailure err

    it "roundtrips a 200 response" $ do
      case decodeHeaderBlock encodeConnectResponse of
        Right headers ->
          lookup ":status" headers `shouldBe` Just "200"
        Left err -> expectationFailure err

    it "roundtrips arbitrary header pairs" $ do
      let headers =
            [ (":method", "CONNECT")
            , (":status", "200")
            , (":scheme", "https")
            ]
          block = encodeHeaderBlock headers
      case decodeHeaderBlock block of
        Right decoded -> decoded `shouldBe` headers
        Left err -> expectationFailure err

  describe "encodeHeaderBlock format" $ do
    it "starts with 2-byte prefix" $ do
      let block = encodeHeaderBlock []
      BS.length block `shouldBe` 2
      BS.index block 0 `shouldBe` 0x00
      BS.index block 1 `shouldBe` 0x00
