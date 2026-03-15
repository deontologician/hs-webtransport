-- | HTTP/3 SETTINGS frame encoding with WebTransport extensions.
--
-- Handles the SETTINGS frame (type 0x04) on the HTTP/3 control stream,
-- including the WebTransport-specific settings:
--
--   * @SETTINGS_ENABLE_WEBTRANSPORT@ (0x2b603742)
--   * @SETTINGS_H3_DATAGRAM@ (0xffd277)
--   * @SETTINGS_ENABLE_CONNECT_PROTOCOL@ (0x08)
--   * @SETTINGS_QPACK_MAX_TABLE_CAPACITY@ (0x01)
--   * @SETTINGS_QPACK_BLOCKED_STREAMS@ (0x07)
module Network.WebTransport.Internal.H3Settings
  ( -- * Settings keys
    H3SettingsKey (..)
  , settingsEnableWebTransport
  , settingsH3Datagram
  , settingsEnableConnectProtocol
  , settingsQpackMaxTableCapacity
  , settingsQpackBlockedStreams
    -- * Encoding / decoding
  , encodeSettings
  , decodeSettings
    -- * H3 frame encoding
  , encodeH3Frame
  , decodeH3Frame
  , H3FrameType (..)
    -- * Defaults
  , defaultWebTransportSettings
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import Network.WebTransport.Internal.VarInt

-- | An HTTP/3 settings key.
newtype H3SettingsKey = H3SettingsKey { unSettingsKey :: Word64 }
  deriving (Eq, Ord, Show)

settingsEnableWebTransport :: H3SettingsKey
settingsEnableWebTransport = H3SettingsKey 0x2b603742

-- | H3_DATAGRAM setting (RFC 9297, §2.1.1).
-- Note: early drafts used 0xffd277; the final RFC value is 0x33.
settingsH3Datagram :: H3SettingsKey
settingsH3Datagram = H3SettingsKey 0x33

settingsEnableConnectProtocol :: H3SettingsKey
settingsEnableConnectProtocol = H3SettingsKey 0x08

settingsQpackMaxTableCapacity :: H3SettingsKey
settingsQpackMaxTableCapacity = H3SettingsKey 0x01

settingsQpackBlockedStreams :: H3SettingsKey
settingsQpackBlockedStreams = H3SettingsKey 0x07

-- | HTTP/3 frame types we care about.
data H3FrameType
  = H3FrameData       -- ^ 0x00
  | H3FrameHeaders    -- ^ 0x01
  | H3FrameSettings   -- ^ 0x04
  | H3FrameGoaway     -- ^ 0x07
  | H3FrameUnknown !Word64
  deriving (Eq, Show)

frameTypeToWord :: H3FrameType -> Word64
frameTypeToWord H3FrameData         = 0x00
frameTypeToWord H3FrameHeaders      = 0x01
frameTypeToWord H3FrameSettings     = 0x04
frameTypeToWord H3FrameGoaway       = 0x07
frameTypeToWord (H3FrameUnknown w)  = w

wordToFrameType :: Word64 -> H3FrameType
wordToFrameType 0x00 = H3FrameData
wordToFrameType 0x01 = H3FrameHeaders
wordToFrameType 0x04 = H3FrameSettings
wordToFrameType 0x07 = H3FrameGoaway
wordToFrameType w    = H3FrameUnknown w

-- | Encode an HTTP/3 frame: type varint ++ length varint ++ payload.
encodeH3Frame :: H3FrameType -> ByteString -> ByteString
encodeH3Frame typ payload =
  encodeVarInt (frameTypeToWord typ)
    <> encodeVarInt (fromIntegral (BS.length payload))
    <> payload

-- | Decode an HTTP/3 frame from a 'ByteString'.
-- Returns (frame type, payload, remaining bytes).
decodeH3Frame :: ByteString -> Either String (H3FrameType, ByteString, ByteString)
decodeH3Frame bs = do
  (typeVal, rest1) <- decodeVarInt bs
  (lenVal, rest2) <- decodeVarInt rest1
  let len = fromIntegral lenVal
  if BS.length rest2 < len
    then Left "decodeH3Frame: insufficient payload bytes"
    else let (payload, rest3) = BS.splitAt len rest2
          in Right (wordToFrameType typeVal, payload, rest3)

-- | Encode a list of settings key-value pairs.
-- Each pair is encoded as two varints (key then value).
encodeSettings :: [(H3SettingsKey, Word64)] -> ByteString
encodeSettings = foldMap (\(H3SettingsKey k, v) -> encodeVarInt k <> encodeVarInt v)

-- | Decode settings key-value pairs from a 'ByteString'.
decodeSettings :: ByteString -> Either String [(H3SettingsKey, Word64)]
decodeSettings bs
  | BS.null bs = Right []
  | otherwise = do
      (k, rest1) <- decodeVarInt bs
      (v, rest2) <- decodeVarInt rest1
      rest <- decodeSettings rest2
      Right ((H3SettingsKey k, v) : rest)

-- | Default settings for a WebTransport endpoint.
--
-- Enables WebTransport, extended CONNECT, and disables QPACK dynamic table
-- (we use static-only encoding for the small set of WebTransport headers).
defaultWebTransportSettings :: [(H3SettingsKey, Word64)]
defaultWebTransportSettings =
  [ (settingsEnableWebTransport, 1)
  , (settingsEnableConnectProtocol, 1)
  , (settingsH3Datagram, 1)
  , (settingsQpackMaxTableCapacity, 0)
  , (settingsQpackBlockedStreams, 0)
  ]
