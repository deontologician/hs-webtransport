-- | WebTransport stream operations.
--
-- Provides send/receive operations over WebTransport streams,
-- which are thin wrappers around QUIC streams with session framing.
module Network.WebTransport.Stream
  ( -- * Types
    SendStream (..)
  , RecvStream (..)
  , BidiStream (..)
  , SessionId (..)
    -- * Operations
  , send
  , sendMany
  , recv
  , finish
  , reset
  , stopSending
  , close
    -- * Splitting
  , split
  ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Network.QUIC as QUIC

-- | Opaque session identifier — the QUIC stream ID of the CONNECT stream.
newtype SessionId = SessionId { unSessionId :: Word64 }
  deriving (Eq, Ord, Show)

-- | A send-only stream within a WebTransport session.
data SendStream = SendStream
  { ssStream    :: !QUIC.Stream
  , ssSessionId :: !SessionId
  }

-- | A receive-only stream within a WebTransport session.
data RecvStream = RecvStream
  { rsStream    :: !QUIC.Stream
  , rsSessionId :: !SessionId
  }

-- | A bidirectional stream within a WebTransport session.
data BidiStream = BidiStream
  { bsStream    :: !QUIC.Stream
  , bsSessionId :: !SessionId
  }

-- | Send data on a stream.
send :: SendStream -> ByteString -> IO ()
send s = QUIC.sendStream (ssStream s)

-- | Send multiple chunks on a stream.
sendMany :: SendStream -> [ByteString] -> IO ()
sendMany s = QUIC.sendStreamMany (ssStream s)

-- | Receive data from a stream. Returns empty 'ByteString' on FIN.
recv :: RecvStream -> Int -> IO ByteString
recv s = QUIC.recvStream (rsStream s)

-- | Signal end of stream (send FIN).
finish :: SendStream -> IO ()
finish = QUIC.shutdownStream . ssStream

-- | Reset a stream with an error code.
reset :: SendStream -> Word64 -> IO ()
reset s code = QUIC.resetStream (ssStream s) (QUIC.ApplicationProtocolError (fromIntegral code))

-- | Stop reading from a stream, asking the peer to stop sending.
stopSending :: RecvStream -> Word64 -> IO ()
stopSending s code = QUIC.stopStream (rsStream s) (QUIC.ApplicationProtocolError (fromIntegral code))

-- | Close a stream (sends FIN if needed).
close :: SendStream -> IO ()
close = QUIC.closeStream . ssStream

-- | Split a bidirectional stream into send and receive halves.
split :: BidiStream -> (SendStream, RecvStream)
split bs =
  ( SendStream (bsStream bs) (bsSessionId bs)
  , RecvStream (bsStream bs) (bsSessionId bs)
  )
