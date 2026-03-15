-- | QUIC variable-length integer encoding (RFC 9000 §16).
--
-- Encodes integers in 1, 2, 4, or 8 bytes depending on magnitude.
-- The two most significant bits of the first byte encode the length.
module Network.WebTransport.Internal.VarInt
  ( encodeVarInt
  , decodeVarInt
  , varIntSize
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word64)

-- | Encode a 'Word64' as a QUIC variable-length integer.
--
-- Values up to 63 use 1 byte, up to 16383 use 2 bytes,
-- up to 1073741823 use 4 bytes, and up to 2^62-1 use 8 bytes.
encodeVarInt :: Word64 -> ByteString
encodeVarInt n
  | n <= 63 =
      BS.singleton (fromIntegral n)
  | n <= 16383 =
      BS.pack
        [ 0x40 .|. fromIntegral (n `shiftR` 8)
        , fromIntegral (n .&. 0xFF)
        ]
  | n <= 1073741823 =
      BS.pack
        [ 0x80 .|. fromIntegral (n `shiftR` 24)
        , fromIntegral ((n `shiftR` 16) .&. 0xFF)
        , fromIntegral ((n `shiftR` 8) .&. 0xFF)
        , fromIntegral (n .&. 0xFF)
        ]
  | n <= 4611686018427387903 =
      BS.pack
        [ 0xC0 .|. fromIntegral (n `shiftR` 56)
        , fromIntegral ((n `shiftR` 48) .&. 0xFF)
        , fromIntegral ((n `shiftR` 40) .&. 0xFF)
        , fromIntegral ((n `shiftR` 32) .&. 0xFF)
        , fromIntegral ((n `shiftR` 24) .&. 0xFF)
        , fromIntegral ((n `shiftR` 16) .&. 0xFF)
        , fromIntegral ((n `shiftR` 8) .&. 0xFF)
        , fromIntegral (n .&. 0xFF)
        ]
  | otherwise = error "encodeVarInt: value exceeds 2^62-1"

-- | Decode a QUIC variable-length integer from the start of a 'ByteString'.
--
-- Returns the decoded value and the remaining bytes, or an error message.
decodeVarInt :: ByteString -> Either String (Word64, ByteString)
decodeVarInt bs
  | BS.null bs = Left "decodeVarInt: empty input"
  | otherwise =
      let b0 = BS.index bs 0
          len = case b0 `shiftR` 6 of
                  0 -> 1
                  1 -> 2
                  2 -> 4
                  _ -> 8
       in if BS.length bs < len
            then Left ("decodeVarInt: need " <> show len <> " bytes, have " <> show (BS.length bs))
            else let val = decodeN len (b0 .&. 0x3F) (BS.drop 1 bs)
                     rest = BS.drop len bs
                  in Right (val, rest)

-- | Compute the encoded size of a value in bytes.
varIntSize :: Word64 -> Int
varIntSize n
  | n <= 63         = 1
  | n <= 16383      = 2
  | n <= 1073741823 = 4
  | otherwise       = 8

-- Internal: decode remaining bytes after stripping the length prefix bits.
decodeN :: Int -> Word8 -> ByteString -> Word64
decodeN len first rest = go 1 (fromIntegral first)
  where
    go i acc
      | i >= len = acc
      | otherwise =
          let b = BS.index rest (i - 1)
           in go (i + 1) (acc `shiftL` 8 .|. fromIntegral b)
