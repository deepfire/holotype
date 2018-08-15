{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, local       ? false
}:
let
  ghc     = import ./ghc.nix { inherit nixpkgs pkgs compiler ghcOrig local; };
  default = import ./default.nix;
in
  ghc.callPackage default {}
