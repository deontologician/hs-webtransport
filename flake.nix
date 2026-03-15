{
  description = "hs-webtransport — WebTransport (RFC 9220) over HTTP/3 for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskell.packages.ghc98;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Haskell
            hsPkgs.ghc
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server

            # Go (for interop tests against webtransport-go)
            pkgs.go

            # Tools
            pkgs.pkg-config
            pkgs.zlib
          ];

          shellHook = ''
            echo "hs-webtransport dev shell — GHC $(ghc --numeric-version)"
          '';
        };
      });
}
