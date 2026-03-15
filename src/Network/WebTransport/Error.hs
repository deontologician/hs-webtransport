-- | Error types for WebTransport.
module Network.WebTransport.Error
  ( WebTransportError (..)
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32, Word64)

-- | Errors that can occur during WebTransport operations.
data WebTransportError
  = -- | The server rejected the session with an HTTP status and reason.
    SessionRejected !Word32 !ByteString
  | -- | The session was closed (CLOSE_WEBTRANSPORT_SESSION capsule).
    SessionClosed !Word32 !ByteString
  | -- | A stream was reset by the peer with an error code.
    StreamError !Word64
  | -- | The peer's SETTINGS did not enable WebTransport.
    SettingsError !Text
  | -- | A protocol-level error (malformed frame, unexpected state, etc.).
    ProtocolError !Text
  deriving (Show, Eq)

instance Exception WebTransportError
