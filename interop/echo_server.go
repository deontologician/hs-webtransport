// WebTransport echo server for interop testing.
//
// Accepts WebTransport sessions, then echoes data on any
// bidirectional stream opened by the client.
//
// Usage:
//
//	go run . --addr 127.0.0.1:0 --cert-out cert.pem --key-out key.pem --addr-out addr.txt
package main

import (
	"context"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"flag"
	"fmt"
	"io"
	"log"
	"math/big"
	"net"
	"net/http"
	"os"
	"time"

	"github.com/quic-go/quic-go/http3"
	"github.com/quic-go/webtransport-go"
)

func main() {
	addr := flag.String("addr", "127.0.0.1:0", "listen address")
	certOut := flag.String("cert-out", "", "write PEM cert to this path")
	keyOut := flag.String("key-out", "", "write PEM key to this path")
	addrOut := flag.String("addr-out", "", "write bound address to this path")
	wtPath := flag.String("path", "/test", "WebTransport endpoint path")
	flag.Parse()

	// Generate self-signed cert
	tlsCert, certPEM, keyPEM, err := generateSelfSignedCert()
	if err != nil {
		log.Fatalf("generate cert: %v", err)
	}

	if *certOut != "" {
		if err := os.WriteFile(*certOut, certPEM, 0644); err != nil {
			log.Fatalf("write cert: %v", err)
		}
	}
	if *keyOut != "" {
		if err := os.WriteFile(*keyOut, keyPEM, 0600); err != nil {
			log.Fatalf("write key: %v", err)
		}
	}

	// Set up WebTransport server
	s := &webtransport.Server{
		H3: http3.Server{
			TLSConfig: &tls.Config{
				Certificates: []tls.Certificate{tlsCert},
			},
		},
		CheckOrigin: func(r *http.Request) bool { return true },
	}

	http.HandleFunc(*wtPath, func(w http.ResponseWriter, r *http.Request) {
		session, err := s.Upgrade(w, r)
		if err != nil {
			log.Printf("upgrade failed: %v", err)
			return
		}
		handleSession(session)
	})

	// Listen on UDP
	udpAddr, err := net.ResolveUDPAddr("udp", *addr)
	if err != nil {
		log.Fatalf("resolve addr: %v", err)
	}
	udpConn, err := net.ListenUDP("udp", udpAddr)
	if err != nil {
		log.Fatalf("listen: %v", err)
	}

	boundAddr := udpConn.LocalAddr().String()
	log.Printf("listening on %s", boundAddr)

	if *addrOut != "" {
		if err := os.WriteFile(*addrOut, []byte(boundAddr), 0644); err != nil {
			log.Fatalf("write addr: %v", err)
		}
	}

	// Serve
	if err := s.Serve(udpConn); err != nil {
		log.Fatalf("serve: %v", err)
	}
}

func handleSession(session *webtransport.Session) {
	defer session.CloseWithError(0, "")
	for {
		stream, err := session.AcceptStream(context.Background())
		if err != nil {
			return
		}
		go echoStream(stream)
	}
}

func echoStream(stream *webtransport.Stream) {
	defer stream.Close()
	_, _ = io.Copy(stream, stream)
}

func generateSelfSignedCert() (tls.Certificate, []byte, []byte, error) {
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return tls.Certificate{}, nil, nil, fmt.Errorf("generate key: %w", err)
	}

	template := x509.Certificate{
		SerialNumber: big.NewInt(1),
		Subject:      pkix.Name{CommonName: "localhost"},
		NotBefore:    time.Now().Add(-time.Hour),
		NotAfter:     time.Now().Add(24 * time.Hour),
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth},
		IPAddresses:  []net.IP{net.ParseIP("127.0.0.1")},
		DNSNames:     []string{"localhost"},
	}

	certDER, err := x509.CreateCertificate(rand.Reader, &template, &template, &key.PublicKey, key)
	if err != nil {
		return tls.Certificate{}, nil, nil, fmt.Errorf("create cert: %w", err)
	}

	certPEM := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: certDER})

	keyDER, err := x509.MarshalECPrivateKey(key)
	if err != nil {
		return tls.Certificate{}, nil, nil, fmt.Errorf("marshal key: %w", err)
	}
	keyPEM := pem.EncodeToMemory(&pem.Block{Type: "EC PRIVATE KEY", Bytes: keyDER})

	tlsCert, err := tls.X509KeyPair(certPEM, keyPEM)
	if err != nil {
		return tls.Certificate{}, nil, nil, fmt.Errorf("x509 key pair: %w", err)
	}

	return tlsCert, certPEM, keyPEM, nil
}
