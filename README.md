# hs-webtransport

A Haskell implementation of [WebTransport](https://www.w3.org/TR/webtransport/) over HTTP/3
([RFC 9220](https://www.rfc-editor.org/rfc/rfc9220)), built on the
[quic](https://hackage.haskell.org/package/quic) package.

WebTransport provides low-latency, bidirectional communication between a client
and server using QUIC streams inside an HTTP/3 connection. Unlike WebSockets,
WebTransport supports multiple independent streams (avoiding head-of-line
blocking), unidirectional streams, and unreliable datagrams.

This library implements the full WebTransport session lifecycle:

1. QUIC connection with ALPN `h3`
2. HTTP/3 control stream + SETTINGS exchange (with WebTransport extensions)
3. Extended CONNECT handshake to establish a WebTransport session
4. Bidirectional and unidirectional stream open/accept with session framing
5. Session closure via CLOSE_WEBTRANSPORT_SESSION capsule

This library was written entirely by [Claude](https://claude.ai) (Anthropic's AI
assistant), using the Rust
[h3-webtransport](https://github.com/hyperium/h3/tree/master/h3-webtransport)
crate as an API reference. Conformance was validated against
[webtransport-go](https://github.com/quic-go/webtransport-go), the reference
implementation used in Chrome/quic-go interop testing --- five protocol issues
were found and fixed through this interop testing.

## Dependencies

**Runtime:**

| Package | Version | Purpose |
|---------|---------|---------|
| `quic` | 0.3.x | QUIC connections, streams, TLS handshake |
| `tls` | 2.x | TLS extension hook for datagram parameter injection |
| `bytestring` | 0.11+ | Wire data |
| `stm` | 2.5+ | Concurrent stream queues |
| `text`, `containers`, `network` | | Standard |

**Test:**

| Package | Purpose |
|---------|---------|
| `hspec` + `hedgehog` | Unit and property tests |
| `webtransport-go` v0.9.0 (Go binary) | Interop conformance tests |

**Build environment:**

```bash
# Enter the Nix dev shell (provides GHC 9.8, Go 1.23, Rust, cabal)
nix develop

# Or if using direnv, it activates automatically
```

## Tutorial

### Client: connect and send data

```haskell
import qualified Network.WebTransport.Client as Client
import qualified Network.WebTransport.Stream as Stream

main :: IO ()
main = do
  let cfg = Client.defaultClientConfig
        { Client.ccServerName = "example.com"
        , Client.ccPort       = 4433
        , Client.ccPath       = "/chat"
        , Client.ccOrigin     = "https://example.com"
        , Client.ccValidate   = True
        }

  Client.connect cfg $ \session -> do
    -- Open a bidirectional stream
    bidi <- Client.openBi session
    let (tx, rx) = Stream.split bidi

    -- Send a message
    Stream.send tx "hello, world"
    Stream.finish tx  -- signal end-of-stream

    -- Read the response
    response <- readAll rx
    putStrLn ("Got: " <> show response)

-- Helper: read all data until FIN
readAll :: Stream.RecvStream -> IO ByteString
readAll rx = go mempty
  where
    go acc = do
      chunk <- Stream.recv rx 4096
      if BS.null chunk
        then pure acc
        else go (acc <> chunk)
```

`connect` handles the full session lifecycle: QUIC connection, HTTP/3 SETTINGS
exchange, and extended CONNECT handshake. When the callback returns, the session
and connection are closed.

### Server: accept sessions and streams

```haskell
import qualified Network.QUIC.Server as QS
import Network.TLS (Credentials(..), credentialLoadX509)
import qualified Network.WebTransport.Server as Server
import qualified Network.WebTransport.Stream as Stream

main :: IO ()
main = do
  -- Load TLS certificate
  Right cred <- credentialLoadX509 "server.pem" "key.pem"

  let cfg = Server.ServerConfig
        { Server.scQuic = QS.defaultServerConfig
            { QS.scAddresses   = [("0.0.0.0", 4433)]
            , QS.scCredentials = Credentials [cred]
            }
        , Server.scAcceptPath = (== "/chat")
        }

  Server.runServer cfg $ \session -> do
    -- Accept bidirectional streams from clients
    bidi <- Server.acceptBi session
    let (tx, rx) = Stream.split bidi

    -- Echo received data
    msg <- Stream.recv rx 4096
    Stream.send tx msg
    Stream.finish tx
```

`runServer` listens for QUIC connections, performs the H3 handshake, and calls
the handler for each WebTransport session (each in its own thread).

### Stream operations

```haskell
import qualified Network.WebTransport.Stream as S

-- Bidirectional: split into send + receive halves
let (tx, rx) = S.split bidi

-- Send data
S.send tx "payload"
S.sendMany tx ["chunk1", "chunk2"]

-- Signal end-of-stream (FIN)
S.finish tx

-- Receive (returns empty ByteString on FIN)
chunk <- S.recv rx 4096

-- Error signaling
S.reset tx errorCode       -- abort sending
S.stopSending rx errorCode -- ask peer to stop
```

### Unidirectional streams

```haskell
-- Client → server (send-only)
tx <- Client.openUni session
Stream.send tx "one-way data"
Stream.finish tx

-- Server → client (receive-only)
rx <- Client.acceptUni session
msg <- Stream.recv rx 4096
```

### Error handling

All WebTransport errors are thrown as `WebTransportError` exceptions:

```haskell
import Network.WebTransport.Error

-- Pattern match on error types
catch action $ \case
  SessionRejected code reason -> ...  -- server rejected CONNECT
  SessionClosed code msg      -> ...  -- CLOSE_WEBTRANSPORT_SESSION received
  StreamError code            -> ...  -- stream reset by peer
  SettingsError msg           -> ...  -- peer doesn't support WebTransport
  ProtocolError msg           -> ...  -- malformed frame, unexpected state
```

## Developing

### Building and testing

```bash
# Build the library
cabal build hs-webtransport

# Run all 44 tests (unit + loopback + interop)
cabal test hs-webtransport-test

# Run only interop tests against webtransport-go
cabal test hs-webtransport-test --test-option='--match=Interop'

# The interop tests need the Go echo server binary:
cd webtransport/interop && go build -o wt-echo-server .
```

### Internal architecture

The library is structured in three layers:

**Wire encoding (`Internal/`)** --- pure functions, no IO:

- `VarInt` --- QUIC variable-length integer encoding (RFC 9000 section 16). Values
  encode in 1, 2, 4, or 8 bytes depending on magnitude. Used everywhere in the
  protocol.

- `H3Settings` --- HTTP/3 SETTINGS frame encode/decode. Handles the
  WebTransport-specific setting keys: `SETTINGS_ENABLE_WEBTRANSPORT` (0x2b603742),
  `SETTINGS_H3_DATAGRAM` (0x33, RFC 9297), and `SETTINGS_ENABLE_CONNECT_PROTOCOL`
  (0x08). Also provides generic H3 frame type/length/value encoding.

- `ExtendedConnect` --- QPACK header block encode/decode for the extended CONNECT
  request and response. Encoding uses static table references where possible and
  literal encoding otherwise (no dynamic table --- we send
  `SETTINGS_QPACK_MAX_TABLE_CAPACITY = 0`). Decoding supports static indexed,
  literal with name reference, and literal with literal name representations,
  plus full Huffman decoding (RFC 7541 Appendix B, 257-symbol code table with
  binary decode tree). The full 99-entry QPACK static table (RFC 9204 Appendix A)
  is included for decoding.

- `SessionStream` --- stream type constants and prefix framing. WebTransport bidi
  streams use frame type `0x41` followed by the session ID varint (which triggers
  the HTTP/3 StreamHijacker in implementations like quic-go). Uni streams use
  stream type `0x54` followed by the session ID.

**Stream abstraction (`Stream`)** --- thin wrappers over `QUIC.Stream` that carry
the session ID. `send`/`recv`/`finish` delegate directly to the quic package.
`split` decomposes a `BidiStream` into `SendStream` + `RecvStream`.

**Session lifecycle (`Client`, `Server`)** --- the complex part:

1. Open a QUIC connection with ALPN `h3`
2. Open three unidirectional streams: H3 control (type 0x00, sends SETTINGS),
   QPACK encoder (type 0x02), QPACK decoder (type 0x03)
3. Client sends an extended CONNECT request on a new bidi stream; server responds
   with 200. The CONNECT stream ID becomes the session ID.
4. New data streams are opened with the session prefix and routed to the session
   via a demuxer thread.
5. Session closure sends a `CLOSE_WEBTRANSPORT_SESSION` capsule (H3 frame type
   0x2843) on the CONNECT stream.

**Datagram parameter workaround:** The `quic` package doesn't support QUIC
datagrams (RFC 9221), but WebTransport peers require seeing the
`max_datagram_frame_size` transport parameter. The client injects this parameter
into the QUIC transport parameters via the `onTLSExtensionCreated` TLS hook,
appending the encoded parameter to the TLS extension bytes. This satisfies the
peer's handshake check without actually supporting datagram frames.

### Test structure

Tests are organized in three tiers:

**Unit tests (37)** --- pure encode/decode roundtrips:
- `VarIntSpec`: property-based roundtrip for arbitrary values, encoding size
  boundaries, RFC 9000 section A.1 known vectors
- `H3SettingsSpec`: settings roundtrip, frame roundtrip, setting key values
- `ExtendedConnectSpec`: QPACK header block roundtrip for CONNECT request/response
- `SessionStreamSpec`: bidi/uni prefix roundtrip, stream type constants
- `ServerSpec`: control stream encoding, capsule encoding
- `ClientSpec`: default config values

**Loopback tests (3)** --- full session lifecycle over localhost:
- Start a minimal H3/WebTransport server using the library's internal modules
- Client connects, opens bidi streams, sends data, server echoes
- Tests: single stream, multiple streams, empty stream (FIN only)

**Interop tests (4)** --- Haskell client against `webtransport-go` v0.9.0 echo server:
- Go server binary built from `webtransport/interop/echo_server.go`
- Tests: bidi echo, multiple streams on one session, 64KB payload, empty stream
- These are the conformance tests. Five protocol issues were found and fixed
  through this testing (see commit `426395b` for details).

### Known limitations

- **No datagram support.** The `quic` package lacks RFC 9221 DATAGRAM frames. We
  advertise `max_datagram_frame_size` in the QUIC handshake (required by peers)
  but cannot actually send or receive datagrams.

- **No QPACK dynamic table.** We send `SETTINGS_QPACK_MAX_TABLE_CAPACITY = 0`
  and only use static table references for encoding. Decoding handles all
  static-table representations and Huffman encoding. Dynamic table references
  in received headers will be rejected.

- **Draft-02 header required.** The `sec-webtransport-http3-draft02: 1` header
  is included in CONNECT requests for compatibility with webtransport-go. This
  will need updating when implementations move to the final RFC.

- **Single session per connection.** The current client API establishes one
  session per `connect` call. Multiple sessions on a single QUIC connection are
  not yet supported.

## License

MIT License

Copyright (c) 2026 Josh Kuhn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
