{ pkgs ? import <nixos> {}
}:

with pkgs;

stdenv.mkDerivation {
  name = "servant-acl";
  buildInputs = [
    stack
    haskell.compiler.ghc921
    zlib
  ];
}
