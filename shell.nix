let nixpkgs = import (builtins.fetchTarball {
      name = "nixos-unstable";
      url = "https://github.com/nixos/nixpkgs/archive/4789953e5c1ef6d10e3ff437e5b7ab8eed526942.tar.gz";
      sha256 = "15nksgi9pncz59l8vrfg05g9dqw835wwdi9bmghffyg0k1yh2j8d";
    }) {};
in

with nixpkgs.pkgs; mkShell {
  buildInputs = [
    cabal-install
    ghc
    gmp
    pkg-config
    secp256k1
  ];
}
