-- | WebTransport session stream framing.
--
-- WebTransport streams carry a session ID prefix that associates them
-- with a particular session (identified by the CONNECT stream's QUIC
-- stream ID).
--
-- * __Bidirectional streams__: first bytes are the session ID varint.
-- * __Unidirectional streams__: first byte is stream type @0x54@,
--   followed by the session ID varint.
--
-- See <https://www.ietf.org/archive/id/draft-ietf-webtrans-http3-09.html#section-4.3>
module Network.WebTransport.Internal.SessionStream
  ( -- * Stream type constants
    wtUniStreamType
  , wtBidiFrameType
    -- * Encoding
  , encodeBidiPrefix
  , encodeUniPrefix
    -- * Decoding
  , decodeBidiPrefix
  , decodeUniPrefix
    -- * Control stream type
  , h3ControlStreamType
  , qpackEncoderStreamType
  , qpackDecoderStreamType
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)

import Network.WebTransport.Internal.VarInt

-- | The WebTransport unidirectional stream type (0x54).
wtUniStreamType :: Word64
wtUniStreamType = 0x54

-- | The WebTransport bidirectional stream frame type (0x41).
-- Used as the first varint on a new bidi stream to signal the HTTP/3
-- layer that this is a WebTransport stream (via StreamHijacker).
wtBidiFrameType :: Word64
wtBidiFrameType = 0x41

-- | HTTP/3 control stream type (0x00).
h3ControlStreamType :: Word64
h3ControlStreamType = 0x00

-- | QPACK encoder stream type (0x02).
qpackEncoderStreamType :: Word64
qpackEncoderStreamType = 0x02

-- | QPACK decoder stream type (0x03).
qpackDecoderStreamType :: Word64
qpackDecoderStreamType = 0x03

-- | Encode the prefix for a WebTransport bidirectional stream.
-- Frame type 0x41 varint followed by session ID varint.
encodeBidiPrefix :: Word64 -> ByteString
encodeBidiPrefix sessionId =
  encodeVarInt wtBidiFrameType <> encodeVarInt sessionId

-- | Encode the prefix for a WebTransport unidirectional stream.
-- Stream type 0x54 varint followed by session ID varint.
encodeUniPrefix :: Word64 -> ByteString
encodeUniPrefix sessionId =
  encodeVarInt wtUniStreamType <> encodeVarInt sessionId

-- | Decode the prefix of a bidirectional stream.
-- Reads frame type varint (must be 0x41), then session ID varint.
-- Returns (session ID, remaining bytes).
decodeBidiPrefix :: ByteString -> Either String (Word64, ByteString)
decodeBidiPrefix bs = do
  (frameType, rest1) <- decodeVarInt bs
  if frameType /= wtBidiFrameType
    then Left ("decodeBidiPrefix: expected frame type 0x41, got " <> show frameType)
    else decodeVarInt rest1

-- | Decode the prefix of a unidirectional stream.
-- Reads the stream type varint first; if it is 0x54, reads the session ID.
-- Returns (stream type, Maybe session ID, remaining bytes).
decodeUniPrefix :: ByteString -> Either String (Word64, Maybe Word64, ByteString)
decodeUniPrefix bs = do
  (streamType, rest1) <- decodeVarInt bs
  if streamType == wtUniStreamType
    then do
      (sessionId, rest2) <- decodeVarInt rest1
      Right (streamType, Just sessionId, rest2)
    else
      Right (streamType, Nothing, rest1)
