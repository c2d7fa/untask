{ pkgs ? (import <nixpkgs>) {} }:
pkgs.stdenv.mkDerivation {
  name = "untask";
  version = "0.5.2";
  src = ./.;
  buildInputs = [ pkgs.racket pkgs.bash pkgs.rlwrap ];
  installPhase = "make install PREFIX=$out";
}


