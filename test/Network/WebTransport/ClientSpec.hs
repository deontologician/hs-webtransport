module Network.WebTransport.ClientSpec (spec) where

import Test.Hspec

import Network.WebTransport.Client (defaultClientConfig, ClientConfig (..))

spec :: Spec
spec = describe "Client" $ do
  describe "defaultClientConfig" $ do
    it "has sensible defaults" $ do
      ccServerName defaultClientConfig `shouldBe` "localhost"
      ccPort defaultClientConfig `shouldBe` 4433
      ccPath defaultClientConfig `shouldBe` "/"
      ccValidate defaultClientConfig `shouldBe` False

  -- Loopback integration tests (client <-> server) are deferred
  -- to InteropSpec once we have a reference server to test against.
  -- The pure encoding/decoding logic is thoroughly tested in the
  -- other spec modules.
