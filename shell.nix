let
  noStrip = next: current: {
    secp256k1 = current.secp256k1.overrideAttrs
      (_: {
        version = "rotibula";
        # dontStrip = true;
      });
  };
  nixpkgs = import <nixpkgs> { overlays = [ noStrip ]; };
in

with nixpkgs.pkgs; mkShell {
  buildInputs = [
    cabal-install
    ghc
    gmp
    pkg-config
    secp256k1
    gdb
  ];
}
