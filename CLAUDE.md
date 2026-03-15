# hs-webtransport

Standalone WebTransport (RFC 9220) library for Haskell, built on the `quic` package.

## What's here

### Public API
- `Network.WebTransport` — Re-exports common types (SessionId, streams, errors)
- `Network.WebTransport.Server` — Server API: accept QUIC connections, run H3 handshake, dispatch WebTransport sessions
- `Network.WebTransport.Client` — Client API: connect, send extended CONNECT, establish sessions
- `Network.WebTransport.Stream` — Stream types (SendStream, RecvStream, BidiStream) and read/write operations
- `Network.WebTransport.Error` — WebTransportError exception type

### Internal modules
- `Internal.VarInt` — QUIC variable-length integer encoding (RFC 9000 §16)
- `Internal.H3Settings` — HTTP/3 SETTINGS frame encoding with WebTransport extension keys
- `Internal.ExtendedConnect` — Extended CONNECT request/response encoding with minimal QPACK (static-only, no dynamic table)
- `Internal.SessionStream` — WebTransport session ID prefix framing for bidi/uni streams

## Design decisions

- **No `http3` dependency.** The `http3` package's public API doesn't expose the hooks needed for WebTransport (custom SETTINGS, extended CONNECT, stream demuxing). We implement the minimal H3 framing ourselves.
- **Static-only QPACK.** WebTransport uses a small, fixed set of headers. We encode them using QPACK static table references and literal encoding, with no dynamic table (`SETTINGS_QPACK_MAX_TABLE_CAPACITY = 0`).
- **No datagrams.** The `quic` package doesn't support RFC 9221 DATAGRAM frames. Stream-based transport works for Rime's needs.

## Building & testing

```bash
# Build
cabal build hs-webtransport

# Run tests (varint, H3 settings, QPACK, session stream framing)
cabal test hs-webtransport-test
```

## Dependencies

Only `quic` (for QUIC connections/streams), `bytestring`, `text`, `containers`, `stm`, and `network`. No `http3` dependency.
