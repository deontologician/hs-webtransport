-- | Extended CONNECT for WebTransport (RFC 9220).
--
-- WebTransport sessions are initiated via an HTTP/3 extended CONNECT request:
--
-- @
--   :method     = CONNECT
--   :protocol   = webtransport
--   :scheme     = https
--   :authority  = host:port
--   :path       = /some/path
-- @
--
-- The server responds with a 200 status to accept.
--
-- We use QPACK static-only encoding (no dynamic table) since the header
-- set is small and fixed. This avoids depending on the @http3@ package.
module Network.WebTransport.Internal.ExtendedConnect
  ( -- * CONNECT request/response
    encodeConnectRequest
  , encodeConnectResponse
  , decodeHeaders
    -- * QPACK static-only encoding
  , encodeHeaderBlock
  , decodeHeaderBlock
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8, Word32)

-- | Encode an extended CONNECT request as a QPACK-encoded header block.
--
-- Returns the header block bytes (to be wrapped in an H3 HEADERS frame).
encodeConnectRequest
  :: ByteString   -- ^ Authority (host:port)
  -> ByteString   -- ^ Path
  -> ByteString   -- ^ Origin header value
  -> ByteString
encodeConnectRequest authority path origin =
  encodeHeaderBlock
    [ (":method", "CONNECT")
    , (":protocol", "webtransport")
    , (":scheme", "https")
    , (":authority", authority)
    , (":path", path)
    , ("origin", origin)
    , ("sec-webtransport-http3-draft02", "1")
    ]

-- | Encode a 200 OK response as a QPACK-encoded header block.
encodeConnectResponse :: ByteString
encodeConnectResponse =
  encodeHeaderBlock [(":status", "200")]

-- | Encode a header list as a QPACK header block using static-only references.
--
-- Format: 2-byte prefix (required insert count = 0, delta base = 0),
-- then each header as either a static table reference or a literal.
--
-- See RFC 9204 §4.5.
encodeHeaderBlock :: [(ByteString, ByteString)] -> ByteString
encodeHeaderBlock headers =
  -- Required Insert Count = 0 (varint, 8-bit prefix) => 0x00
  -- S=0, Delta Base = 0 (varint, 7-bit prefix) => 0x00
  BS.pack [0x00, 0x00] <> foldMap encodeHeader headers

-- | Encode a single header field.
encodeHeader :: (ByteString, ByteString) -> ByteString
encodeHeader (name, value) =
  case lookupStaticExact name value of
    Just idx -> encodeStaticIndexed idx
    Nothing  -> case lookupStaticName name of
      Just idx -> encodeStaticNameRef idx value
      Nothing  -> encodeLiteral name value

-- | Encode a fully-indexed header from the static table.
-- Format: 1_T_XXXXXX (indexed field line, T=1 static, 6-bit prefix index)
encodeStaticIndexed :: Int -> ByteString
encodeStaticIndexed idx = encodePrefixedInt 0xC0 6 idx

-- | Encode a header with static name reference and literal value.
-- Format: 0_1_N_T_XXXX (literal with name ref, N=0 not-never-indexed, T=1 static, 4-bit prefix)
encodeStaticNameRef :: Int -> ByteString -> ByteString
encodeStaticNameRef idx value =
  encodePrefixedInt 0x50 4 idx <> encodeLiteralValue value

-- | Encode a fully literal header (no table reference).
-- Format: 0_0_1_N_H_XXX (literal name, N=0, H=0 no Huffman, 3-bit prefix name len)
encodeLiteral :: ByteString -> ByteString -> ByteString
encodeLiteral name value =
  encodePrefixedInt 0x20 3 (BS.length name) <> name <> encodeLiteralValue value

-- | Encode a literal string value (H=0 no Huffman, 7-bit prefix length).
encodeLiteralValue :: ByteString -> ByteString
encodeLiteralValue value =
  encodePrefixedInt 0x00 7 (BS.length value) <> value

-- | HPACK/QPACK integer encoding (RFC 7541 §5.1).
--
-- @encodePrefixedInt highBits prefixBits value@ encodes @value@ using
-- @prefixBits@ low bits of the first byte, with @highBits@ as the
-- fixed pattern in the remaining high bits.
encodePrefixedInt :: Word8 -> Int -> Int -> ByteString
encodePrefixedInt highBits prefixBits value
  | value < maxPrefix =
      BS.singleton (highBits .|. fromIntegral value)
  | otherwise =
      BS.singleton (highBits .|. fromIntegral maxPrefix)
        <> encodeOverflow (value - maxPrefix)
  where
    maxPrefix = (1 `shiftL` prefixBits) - 1

encodeOverflow :: Int -> ByteString
encodeOverflow n
  | n < 128   = BS.singleton (fromIntegral n)
  | otherwise =
      BS.singleton (0x80 .|. fromIntegral (n .&. 0x7F))
        <> encodeOverflow (n `shiftR` 7)

-- | Decode a QPACK header block into header pairs.
--
-- Supports the subset needed for WebTransport:
-- static indexed, static name-ref + literal value, and fully literal.
decodeHeaderBlock :: ByteString -> Either String [(ByteString, ByteString)]
decodeHeaderBlock bs
  | BS.length bs < 2 = Left "decodeHeaderBlock: too short for prefix"
  | otherwise = go (BS.drop 2 bs) []
  where
    go rest acc
      | BS.null rest = Right (reverse acc)
      | otherwise = do
          (hdr, rest') <- decodeOneHeader rest
          go rest' (hdr : acc)

decodeOneHeader :: ByteString -> Either String ((ByteString, ByteString), ByteString)
decodeOneHeader bs
  | BS.null bs = Left "decodeOneHeader: empty"
  | b0 .&. 0x80 /= 0 = decodeIndexed bs          -- 1xxxxxxx: indexed field line
  | b0 .&. 0xC0 == 0x40 = decodeLiteralNameRef bs -- 01xxxxxx: literal with name ref
  | b0 .&. 0xE0 == 0x20 = decodeLiteralFull bs    -- 001xxxxx: literal with literal name
  | b0 .&. 0xF0 == 0x10 = Left "decodeOneHeader: indexed post-base not supported"
  | otherwise = Left ("decodeOneHeader: unknown pattern: " <> show b0)
  where
    b0 = BS.index bs 0

-- | Decode a prefixed integer (RFC 7541 §5.1 / RFC 9204 §4.1.1).
-- Takes the first byte (already read), the number of prefix bits, and
-- the remaining bytes. Returns (decoded value, remaining bytes).
decodePrefixedInt :: Word8 -> Int -> ByteString -> Either String (Int, ByteString)
decodePrefixedInt firstByte prefixBits rest
  | prefixVal < maxPrefix = Right (prefixVal, rest)
  | otherwise = decodeOverflow maxPrefix 0 0 rest
  where
    mask = (1 `shiftL` prefixBits) - 1
    prefixVal = fromIntegral (firstByte .&. fromIntegral mask)
    maxPrefix = mask

decodeOverflow :: Int -> Int -> Int -> ByteString -> Either String (Int, ByteString)
decodeOverflow acc shift _count bs
  | BS.null bs = Left "decodePrefixedInt: premature end during overflow"
  | otherwise =
      let b = BS.index bs 0
          rest = BS.drop 1 bs
          contribution = fromIntegral (b .&. 0x7F) `shiftL` shift
          acc' = acc + contribution
       in if testBit b 7
            then decodeOverflow acc' (shift + 7) 0 rest
            else Right (acc', rest)

-- Decode a static indexed field (1_T_XXXXXX, T=1 for static, 6-bit prefix).
decodeIndexed :: ByteString -> Either String ((ByteString, ByteString), ByteString)
decodeIndexed bs = do
  let b0 = BS.index bs 0
  if not (testBit b0 6)
    then Left "decodeIndexed: dynamic table references not supported"
    else do
      (idx, rest) <- decodePrefixedInt b0 6 (BS.drop 1 bs)
      case lookupStaticEntry idx of
        Just (n, v) -> Right ((n, v), rest)
        Nothing -> Left ("decodeIndexed: unknown static index " <> show idx)

-- Decode literal with static name reference (0_1_N_T_XXXX, 4-bit prefix).
decodeLiteralNameRef :: ByteString -> Either String ((ByteString, ByteString), ByteString)
decodeLiteralNameRef bs = do
  let b0 = BS.index bs 0
  if not (testBit b0 4)
    then Left "decodeLiteralNameRef: dynamic table references not supported"
    else do
      (idx, rest1) <- decodePrefixedInt b0 4 (BS.drop 1 bs)
      name <- case lookupStaticEntry idx of
        Just (n, _) -> Right n
        Nothing -> Left ("decodeLiteralNameRef: unknown static index " <> show idx)
      (value, rest2) <- decodeLiteralString rest1
      Right ((name, value), rest2)

-- Decode fully literal header (0_0_1_N_H_XXX, 3-bit prefix name length).
decodeLiteralFull :: ByteString -> Either String ((ByteString, ByteString), ByteString)
decodeLiteralFull bs = do
  let b0 = BS.index bs 0
  (nameLen, rest1) <- decodePrefixedInt b0 3 (BS.drop 1 bs)
  if BS.length rest1 < nameLen
    then Left "decodeLiteralFull: not enough bytes for name"
    else do
      let (name, rest2) = BS.splitAt nameLen rest1
      (value, rest3) <- decodeLiteralString rest2
      Right ((name, value), rest3)

-- Decode a literal string (7-bit prefix length, with optional Huffman encoding).
decodeLiteralString :: ByteString -> Either String (ByteString, ByteString)
decodeLiteralString bs
  | BS.null bs = Left "decodeLiteralString: empty"
  | otherwise =
      let b0 = BS.index bs 0
          huffman = testBit b0 7
       in do
            (len, rest) <- decodePrefixedInt b0 7 (BS.drop 1 bs)
            if BS.length rest < len
              then Left "decodeLiteralString: not enough bytes for value"
              else
                let (encoded, rest') = BS.splitAt len rest
                 in if huffman
                      then do
                        decoded <- decodeHuffman encoded
                        Right (decoded, rest')
                      else Right (encoded, rest')

-- | Decode an HPACK Huffman-coded string (RFC 7541 Appendix B).
--
-- Reads bits from the input, walks the Huffman decode tree to emit symbols.
-- Padding (up to 7 bits of 1s) is accepted. EOS symbol or non-1 padding is
-- rejected.
decodeHuffman :: ByteString -> Either String ByteString
decodeHuffman input
  | BS.null input = Right BS.empty
  | otherwise = go 0 0 0 huffmanRoot 0 True mempty
  where
    inputLen = BS.length input

    -- @padBits@ counts how many bits consumed since the last symbol emission.
    -- @allOnes@ tracks whether all those bits were 1.
    -- At end-of-input, remaining bits are padding: must be all-1s and <= 7.
    go :: Int -> Word8 -> Int -> HuffmanNode -> Int -> Bool -> Builder.Builder
       -> Either String ByteString
    go !byteIdx !currentByte !bitIdx !node !padBits !allOnes !acc
      | byteIdx >= inputLen =
          case node of
            HLeaf _ _ -> Left "decodeHuffman: incomplete symbol at end of input"
            HBranch _ _
              | padBits == 0 -> Right (LBS.toStrict (Builder.toLazyByteString acc))
              | padBits > 7  -> Left "decodeHuffman: padding exceeds 7 bits"
              | not allOnes  -> Left "decodeHuffman: invalid padding"
              | otherwise    -> Right (LBS.toStrict (Builder.toLazyByteString acc))
      | otherwise =
          let byte = if bitIdx == 0
                       then BS.index input byteIdx
                       else currentByte
              bit = testBit byte (7 - bitIdx)
              (byteIdx', bitIdx') = if bitIdx == 7
                                      then (byteIdx + 1, 0)
                                      else (byteIdx, bitIdx + 1)
           in case node of
                HLeaf _ _ -> Left "decodeHuffman: unexpected leaf (should not happen)"
                HBranch zeroBranch oneBranch ->
                  let next = if bit then oneBranch else zeroBranch
                   in case next of
                        HLeaf 256 _ -> Left "decodeHuffman: EOS symbol in input"
                        HLeaf sym _ -> go byteIdx' byte bitIdx' huffmanRoot 0 True
                                          (acc <> Builder.word8 (fromIntegral sym))
                        HBranch _ _ -> go byteIdx' byte bitIdx' next
                                          (padBits + 1) (allOnes && bit) acc

-- | Huffman decode tree node.
data HuffmanNode
  = HLeaf !Int !Int                  -- ^ Symbol (0-256), bit length
  | HBranch !HuffmanNode !HuffmanNode -- ^ zero-branch, one-branch
  deriving (Show)

-- | Build the Huffman decode tree (computed once via top-level binding).
huffmanRoot :: HuffmanNode
huffmanRoot = buildHuffmanTree huffmanTable

buildHuffmanTree :: [(Int, Word32, Int)] -> HuffmanNode
buildHuffmanTree entries = foldl insertEntry emptyBranch entries
  where
    emptyBranch = HBranch emptyLeaf emptyLeaf
    emptyLeaf = HLeaf (-1) 0  -- placeholder, should never be reached

    insertEntry :: HuffmanNode -> (Int, Word32, Int) -> HuffmanNode
    insertEntry root (sym, code, bitLen) = insert root (bitLen - 1)
      where
        insert (HBranch zero one) pos
          | pos < 0 = HLeaf sym bitLen
          | testBit code pos =
              HBranch zero (insert one (pos - 1))
          | otherwise =
              HBranch (insert zero (pos - 1)) one
        insert (HLeaf _ _) pos
          | pos < 0 = HLeaf sym bitLen
          | otherwise =
              -- Expanding a placeholder leaf into a branch
              let branch = HBranch emptyLeaf emptyLeaf
               in insert branch pos
        {-# INLINE insert #-}

-- | HPACK Huffman code table (RFC 7541 Appendix B).
-- Each entry is (symbol, code, bit_length).
-- Symbol 256 is EOS.
huffmanTable :: [(Int, Word32, Int)]
huffmanTable =
  [ (  0, 0x1ff8,     13)
  , (  1, 0x7fffd8,   23)
  , (  2, 0xfffffe2,  28)
  , (  3, 0xfffffe3,  28)
  , (  4, 0xfffffe4,  28)
  , (  5, 0xfffffe5,  28)
  , (  6, 0xfffffe6,  28)
  , (  7, 0xfffffe7,  28)
  , (  8, 0xfffffe8,  28)
  , (  9, 0xffffea,   24)
  , ( 10, 0x3ffffffc, 30)
  , ( 11, 0xfffffe9,  28)
  , ( 12, 0xfffffea,  28)
  , ( 13, 0x3ffffffd, 30)
  , ( 14, 0xfffffeb,  28)
  , ( 15, 0xfffffec,  28)
  , ( 16, 0xfffffed,  28)
  , ( 17, 0xfffffee,  28)
  , ( 18, 0xfffffef,  28)
  , ( 19, 0xffffff0,  28)
  , ( 20, 0xffffff1,  28)
  , ( 21, 0xffffff2,  28)
  , ( 22, 0x3ffffffe, 30)
  , ( 23, 0xffffff3,  28)
  , ( 24, 0xffffff4,  28)
  , ( 25, 0xffffff5,  28)
  , ( 26, 0xffffff6,  28)
  , ( 27, 0xffffff7,  28)
  , ( 28, 0xffffff8,  28)
  , ( 29, 0xffffff9,  28)
  , ( 30, 0xffffffa,  28)
  , ( 31, 0xffffffb,  28)
  , ( 32, 0x14,        6)
  , ( 33, 0x3f8,      10)
  , ( 34, 0x3f9,      10)
  , ( 35, 0xffa,      12)
  , ( 36, 0x1ff9,     13)
  , ( 37, 0x15,        6)
  , ( 38, 0xf8,        8)
  , ( 39, 0x7fa,      11)
  , ( 40, 0x3fa,      10)
  , ( 41, 0x3fb,      10)
  , ( 42, 0xf9,        8)
  , ( 43, 0x7fb,      11)
  , ( 44, 0xfa,        8)
  , ( 45, 0x16,        6)
  , ( 46, 0x17,        6)
  , ( 47, 0x18,        6)
  , ( 48, 0x0,         5)
  , ( 49, 0x1,         5)
  , ( 50, 0x2,         5)
  , ( 51, 0x19,        6)
  , ( 52, 0x1a,        6)
  , ( 53, 0x1b,        6)
  , ( 54, 0x1c,        6)
  , ( 55, 0x1d,        6)
  , ( 56, 0x1e,        6)
  , ( 57, 0x1f,        6)
  , ( 58, 0x5c,        7)
  , ( 59, 0xfb,        8)
  , ( 60, 0x7ffc,     15)
  , ( 61, 0x20,        6)
  , ( 62, 0xffb,      12)
  , ( 63, 0x3fc,      10)
  , ( 64, 0x1ffa,     13)
  , ( 65, 0x21,        6)
  , ( 66, 0x5d,        7)
  , ( 67, 0x5e,        7)
  , ( 68, 0x5f,        7)
  , ( 69, 0x60,        7)
  , ( 70, 0x61,        7)
  , ( 71, 0x62,        7)
  , ( 72, 0x63,        7)
  , ( 73, 0x64,        7)
  , ( 74, 0x65,        7)
  , ( 75, 0x66,        7)
  , ( 76, 0x67,        7)
  , ( 77, 0x68,        7)
  , ( 78, 0x69,        7)
  , ( 79, 0x6a,        7)
  , ( 80, 0x6b,        7)
  , ( 81, 0x6c,        7)
  , ( 82, 0x6d,        7)
  , ( 83, 0x6e,        7)
  , ( 84, 0x6f,        7)
  , ( 85, 0x70,        7)
  , ( 86, 0x71,        7)
  , ( 87, 0x72,        7)
  , ( 88, 0xfc,        8)
  , ( 89, 0x73,        7)
  , ( 90, 0xfd,        8)
  , ( 91, 0x1ffb,     13)
  , ( 92, 0x7fff0,    19)
  , ( 93, 0x1ffc,     13)
  , ( 94, 0x3ffc,     14)
  , ( 95, 0x22,        6)
  , ( 96, 0x7ffd,     15)
  , ( 97, 0x3,         5)
  , ( 98, 0x23,        6)
  , ( 99, 0x4,         5)
  , (100, 0x24,        6)
  , (101, 0x5,         5)
  , (102, 0x25,        6)
  , (103, 0x26,        6)
  , (104, 0x27,        6)
  , (105, 0x6,         5)
  , (106, 0x74,        7)
  , (107, 0x75,        7)
  , (108, 0x28,        6)
  , (109, 0x29,        6)
  , (110, 0x2a,        6)
  , (111, 0x7,         5)
  , (112, 0x2b,        6)
  , (113, 0x76,        7)
  , (114, 0x2c,        6)
  , (115, 0x8,         5)
  , (116, 0x9,         5)
  , (117, 0x2d,        6)
  , (118, 0x77,        7)
  , (119, 0x78,        7)
  , (120, 0x79,        7)
  , (121, 0x7a,        7)
  , (122, 0x7b,        7)
  , (123, 0x7ffe,     15)
  , (124, 0x7fc,      11)
  , (125, 0x3ffd,     14)
  , (126, 0x1ffd,     13)
  , (127, 0xffffffc,  28)
  , (128, 0xfffe6,    20)
  , (129, 0x3fffd2,   22)
  , (130, 0xfffe7,    20)
  , (131, 0xfffe8,    20)
  , (132, 0x3fffd3,   22)
  , (133, 0x3fffd4,   22)
  , (134, 0x3fffd5,   22)
  , (135, 0x7fffd9,   23)
  , (136, 0x3fffd6,   22)
  , (137, 0x7fffda,   23)
  , (138, 0x7fffdb,   23)
  , (139, 0x7fffdc,   23)
  , (140, 0x7fffdd,   23)
  , (141, 0x7fffde,   23)
  , (142, 0xffffeb,   24)
  , (143, 0x7fffdf,   23)
  , (144, 0xffffec,   24)
  , (145, 0xffffed,   24)
  , (146, 0x3fffd7,   22)
  , (147, 0x7fffe0,   23)
  , (148, 0xffffee,   24)
  , (149, 0x7fffe1,   23)
  , (150, 0x7fffe2,   23)
  , (151, 0x7fffe3,   23)
  , (152, 0x7fffe4,   23)
  , (153, 0x1fffdc,   21)
  , (154, 0x3fffd8,   22)
  , (155, 0x7fffe5,   23)
  , (156, 0x3fffd9,   22)
  , (157, 0x7fffe6,   23)
  , (158, 0x7fffe7,   23)
  , (159, 0xffffef,   24)
  , (160, 0x3fffda,   22)
  , (161, 0x1fffdd,   21)
  , (162, 0xfffe9,    20)
  , (163, 0x3fffdb,   22)
  , (164, 0x3fffdc,   22)
  , (165, 0x7fffe8,   23)
  , (166, 0x7fffe9,   23)
  , (167, 0x1fffde,   21)
  , (168, 0x7fffea,   23)
  , (169, 0x3fffdd,   22)
  , (170, 0x3fffde,   22)
  , (171, 0xfffff0,   24)
  , (172, 0x1fffdf,   21)
  , (173, 0x3fffdf,   22)
  , (174, 0x7fffeb,   23)
  , (175, 0x7fffec,   23)
  , (176, 0x1fffe0,   21)
  , (177, 0x1fffe1,   21)
  , (178, 0x3fffe0,   22)
  , (179, 0x1fffe2,   21)
  , (180, 0x7fffed,   23)
  , (181, 0x3fffe1,   22)
  , (182, 0x7fffee,   23)
  , (183, 0x7fffef,   23)
  , (184, 0xfffea,    20)
  , (185, 0x3fffe2,   22)
  , (186, 0x3fffe3,   22)
  , (187, 0x3fffe4,   22)
  , (188, 0x7ffff0,   23)
  , (189, 0x3fffe5,   22)
  , (190, 0x3fffe6,   22)
  , (191, 0x7ffff1,   23)
  , (192, 0x3ffffe0,  26)
  , (193, 0x3ffffe1,  26)
  , (194, 0xfffeb,    20)
  , (195, 0x7fff1,    19)
  , (196, 0x3fffe7,   22)
  , (197, 0x7ffff2,   23)
  , (198, 0x3fffe8,   22)
  , (199, 0x1ffffec,  25)
  , (200, 0x3ffffe2,  26)
  , (201, 0x3ffffe3,  26)
  , (202, 0x3ffffe4,  26)
  , (203, 0x7ffffde,  27)
  , (204, 0x7ffffdf,  27)
  , (205, 0x3ffffe5,  26)
  , (206, 0xfffff1,   24)
  , (207, 0x1ffffed,  25)
  , (208, 0x7fff2,    19)
  , (209, 0x1fffe3,   21)
  , (210, 0x3ffffe6,  26)
  , (211, 0x7ffffe0,  27)
  , (212, 0x7ffffe1,  27)
  , (213, 0x3ffffe7,  26)
  , (214, 0x7ffffe2,  27)
  , (215, 0xfffff2,   24)
  , (216, 0x1fffe4,   21)
  , (217, 0x1fffe5,   21)
  , (218, 0x3ffffe8,  26)
  , (219, 0x3ffffe9,  26)
  , (220, 0xffffffd,  28)
  , (221, 0x7ffffe3,  27)
  , (222, 0x7ffffe4,  27)
  , (223, 0x7ffffe5,  27)
  , (224, 0xfffec,    20)
  , (225, 0xfffff3,   24)
  , (226, 0xfffed,    20)
  , (227, 0x1fffe6,   21)
  , (228, 0x3fffe9,   22)
  , (229, 0x1fffe7,   21)
  , (230, 0x1fffe8,   21)
  , (231, 0x7ffff3,   23)
  , (232, 0x3fffea,   22)
  , (233, 0x3fffeb,   22)
  , (234, 0x1ffffee,  25)
  , (235, 0x1ffffef,  25)
  , (236, 0xfffff4,   24)
  , (237, 0xfffff5,   24)
  , (238, 0x3ffffea,  26)
  , (239, 0x7ffff4,   23)
  , (240, 0x3ffffeb,  26)
  , (241, 0x7ffffe6,  27)
  , (242, 0x3ffffec,  26)
  , (243, 0x3ffffed,  26)
  , (244, 0x7ffffe7,  27)
  , (245, 0x7ffffe8,  27)
  , (246, 0x7ffffe9,  27)
  , (247, 0x7ffffea,  27)
  , (248, 0x7ffffeb,  27)
  , (249, 0xffffffe,  28)
  , (250, 0x7ffffec,  27)
  , (251, 0x7ffffed,  27)
  , (252, 0x7ffffee,  27)
  , (253, 0x7ffffef,  27)
  , (254, 0x7fffff0,  27)
  , (255, 0x3ffffee,  26)
  , (256, 0x3fffffff, 30)  -- EOS
  ]

-- | Decode all headers from a raw header block (for responses/requests).
decodeHeaders :: ByteString -> Either String [(ByteString, ByteString)]
decodeHeaders = decodeHeaderBlock

-- QPACK static table (RFC 9204 §Appendix A) — entries we care about.
-- Only the entries needed for WebTransport.

lookupStaticExact :: ByteString -> ByteString -> Maybe Int
lookupStaticExact name value = go 0 qpackStaticTable
  where
    go _ [] = Nothing
    go i ((n, v) : rest)
      | n == name && v == value = Just i
      | otherwise = go (i + 1) rest

lookupStaticName :: ByteString -> Maybe Int
lookupStaticName name = go 0 qpackStaticTable
  where
    go _ [] = Nothing
    go i ((n, _) : rest)
      | n == name = Just i
      | otherwise = go (i + 1) rest

-- | Full QPACK static table (RFC 9204, Appendix A).
-- All 99 entries (indices 0–98).
lookupStaticEntry :: Int -> Maybe (ByteString, ByteString)
lookupStaticEntry i
  | i >= 0 && i < length qpackStaticTable = Just (qpackStaticTable !! i)
  | otherwise = Nothing

qpackStaticTable :: [(ByteString, ByteString)]
qpackStaticTable =
  [ (":authority", "")                             -- 0
  , (":path", "/")                                 -- 1
  , ("age", "0")                                   -- 2
  , ("content-disposition", "")                    -- 3
  , ("content-length", "0")                        -- 4
  , ("cookie", "")                                 -- 5
  , ("date", "")                                   -- 6
  , ("etag", "")                                   -- 7
  , ("if-modified-since", "")                      -- 8
  , ("if-none-match", "")                          -- 9
  , ("last-modified", "")                          -- 10
  , ("link", "")                                   -- 11
  , ("location", "")                               -- 12
  , ("referer", "")                                -- 13
  , ("set-cookie", "")                             -- 14
  , (":method", "CONNECT")                         -- 15
  , (":method", "DELETE")                          -- 16
  , (":method", "GET")                             -- 17
  , (":method", "HEAD")                            -- 18
  , (":method", "OPTIONS")                         -- 19
  , (":method", "POST")                            -- 20
  , (":method", "PUT")                             -- 21
  , (":scheme", "http")                            -- 22
  , (":scheme", "https")                           -- 23
  , (":status", "103")                             -- 24
  , (":status", "200")                             -- 25
  , (":status", "304")                             -- 26
  , (":status", "404")                             -- 27
  , (":status", "503")                             -- 28
  , ("accept", "*/*")                              -- 29
  , ("accept", "application/dns-message")          -- 30
  , ("accept-encoding", "gzip, deflate, br")       -- 31
  , ("accept-ranges", "bytes")                     -- 32
  , ("access-control-allow-headers", "cache-control")     -- 33
  , ("access-control-allow-headers", "content-type")      -- 34
  , ("access-control-allow-origin", "*")           -- 35
  , ("cache-control", "max-age=0")                 -- 36
  , ("cache-control", "max-age=2592000")           -- 37
  , ("cache-control", "max-age=604800")            -- 38
  , ("cache-control", "no-cache")                  -- 39
  , ("cache-control", "no-store")                  -- 40
  , ("cache-control", "public, max-age=31536000")  -- 41
  , ("content-encoding", "br")                     -- 42
  , ("content-encoding", "gzip")                   -- 43
  , ("content-type", "application/dns-message")    -- 44
  , ("content-type", "application/javascript")     -- 45
  , ("content-type", "application/json")           -- 46
  , ("content-type", "application/x-www-form-urlencoded") -- 47
  , ("content-type", "image/gif")                  -- 48
  , ("content-type", "image/jpeg")                 -- 49
  , ("content-type", "image/png")                  -- 50
  , ("content-type", "text/css")                   -- 51
  , ("content-type", "text/html; charset=utf-8")   -- 52
  , ("content-type", "text/plain")                 -- 53
  , ("content-type", "text/plain;charset=utf-8")   -- 54
  , ("range", "bytes=0-")                          -- 55
  , ("strict-transport-security", "max-age=31536000")                  -- 56
  , ("strict-transport-security", "max-age=31536000; includesubdomains") -- 57
  , ("strict-transport-security", "max-age=31536000; includesubdomains; preload") -- 58
  , ("vary", "accept-encoding")                    -- 59
  , ("vary", "origin")                             -- 60
  , ("x-content-type-options", "nosniff")          -- 61
  , ("x-xss-protection", "1; mode=block")          -- 62
  , (":status", "100")                             -- 63
  , (":status", "204")                             -- 64
  , (":status", "206")                             -- 65
  , (":status", "302")                             -- 66
  , (":status", "400")                             -- 67
  , (":status", "403")                             -- 68
  , (":status", "421")                             -- 69
  , (":status", "425")                             -- 70
  , (":status", "500")                             -- 71
  , ("accept-language", "")                        -- 72
  , ("access-control-allow-credentials", "FALSE")  -- 73
  , ("access-control-allow-credentials", "TRUE")   -- 74
  , ("access-control-allow-headers", "*")          -- 75
  , ("access-control-allow-methods", "get")        -- 76
  , ("access-control-allow-methods", "get, post, options") -- 77
  , ("access-control-allow-methods", "options")    -- 78
  , ("access-control-expose-headers", "content-length")   -- 79
  , ("access-control-request-headers", "content-type")    -- 80
  , ("access-control-request-method", "get")       -- 81
  , ("access-control-request-method", "post")      -- 82
  , ("alt-svc", "clear")                           -- 83
  , ("authorization", "")                          -- 84
  , ("content-security-policy", "script-src 'none'; object-src 'none'; base-uri 'none'") -- 85
  , ("early-data", "1")                            -- 86
  , ("expect-ct", "")                              -- 87
  , ("forwarded", "")                              -- 88
  , ("if-range", "")                               -- 89
  , ("origin", "")                                 -- 90
  , ("purpose", "prefetch")                        -- 91
  , ("server", "")                                 -- 92
  , ("timing-allow-origin", "*")                   -- 93
  , ("upgrade-insecure-requests", "1")             -- 94
  , ("user-agent", "")                             -- 95
  , ("x-forwarded-for", "")                        -- 96
  , ("x-frame-options", "deny")                    -- 97
  , ("x-frame-options", "sameorigin")              -- 98
  ]
