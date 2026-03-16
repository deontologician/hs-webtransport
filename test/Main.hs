module Main (main) where

import Test.Hspec

import qualified Network.WebTransport.VarIntSpec
import qualified Network.WebTransport.H3SettingsSpec
import qualified Network.WebTransport.SessionStreamSpec
import qualified Network.WebTransport.ExtendedConnectSpec
import qualified Network.WebTransport.ServerSpec
import qualified Network.WebTransport.ClientSpec
import qualified Network.WebTransport.LoopbackSpec
import qualified Network.WebTransport.InteropSpec
import qualified Network.WebTransport.ConformanceSpec

main :: IO ()
main = hspec $ do
  Network.WebTransport.VarIntSpec.spec
  Network.WebTransport.H3SettingsSpec.spec
  Network.WebTransport.SessionStreamSpec.spec
  Network.WebTransport.ExtendedConnectSpec.spec
  Network.WebTransport.ServerSpec.spec
  Network.WebTransport.ClientSpec.spec
  Network.WebTransport.LoopbackSpec.spec
  Network.WebTransport.InteropSpec.spec
  Network.WebTransport.ConformanceSpec.spec
