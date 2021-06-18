{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "servant-acl";
  buildInputs = [
    stack
    haskell.compiler.ghc8104
    nodejs
    zlib
  ];
}
