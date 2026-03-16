-- | WebTransport over HTTP/3 for Haskell (draft-ietf-webtrans-http3).
--
-- This module re-exports common types. For the full API, import:
--
-- * "Network.WebTransport.Server" — server-side API
-- * "Network.WebTransport.Client" — client-side API
-- * "Network.WebTransport.Stream" — stream operations
-- * "Network.WebTransport.Error"  — error types
module Network.WebTransport
  ( -- * Session identifier
    SessionId (..)
    -- * Stream types
  , SendStream (..)
  , RecvStream (..)
  , BidiStream (..)
    -- * Stream operations
  , Stream.send
  , Stream.sendMany
  , Stream.recv
  , Stream.finish
  , Stream.reset
  , Stream.stopSending
  , Stream.close
  , Stream.split
    -- * Errors
  , WebTransportError (..)
  ) where

import Network.WebTransport.Error (WebTransportError (..))
import Network.WebTransport.Stream (BidiStream (..), RecvStream (..), SendStream (..), SessionId (..))
import qualified Network.WebTransport.Stream as Stream
