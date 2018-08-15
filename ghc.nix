{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, local       ? false
}:

let
  overlays = [
    (_: pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = import ./overrides.nix        { inherit pkgs; };
      };
    })
    (_: pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = import ./manual-overrides.nix { inherit pkgs local; };
      };
    })
  ];
in
  (import <nixpkgs> { inherit overlays; }).haskellPackages
