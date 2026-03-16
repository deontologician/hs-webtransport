# Changelog

## 0.1.0.0 — 2026-03-15

Initial release.

- Client and server APIs for WebTransport over HTTP/3 (draft-ietf-webtrans-http3-15)
- Session establishment via Extended CONNECT (RFC 9220)
- Bidirectional and unidirectional stream support
- Multiple sessions per connection (`withConnection` + `newSession`)
- Static-only QPACK encoding (no dynamic table)
- Built on the `quic` package; no `http3` dependency
