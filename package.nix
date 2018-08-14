{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, local       ? false
}:
let

  ghc     = import ./ghc.nix { inherit nixpkgs pkgs haskell compiler ghcOrig local; };
  default = import ./default.nix;
in
  ghc.callPackage default {}
