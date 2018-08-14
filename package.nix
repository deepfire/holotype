{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, local       ? false
}:
let

  ghc     = import ./packages.nix { inherit nixpkgs pkgs haskell compiler ghcOrig local; };
  default = import ./.;
in
  ghc.callPackage default {}
