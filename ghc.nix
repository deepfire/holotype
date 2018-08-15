{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, local       ? false
}:

let
  lib = {
    debugBuild = pkg: pkgs.haskell.lib.overrideCabal pkg (drv: {
      configureFlags  = "--ghc-option=-g --ghc-option=-O1";
      doHaddock       = false;
      dontStrip       = true;
    });
  };
  overlays = [
    (_: pkgs: {
      haskellPackages = pkgs.haskellPackages.override (oldArgs: {
        overrides = self: super:
                    let parent = (oldArgs.overrides or (_: _: {})) self super;
                    in parent // import ./overrides.nix         { inherit self super pkgs lib; };
      });
    })
    (_: pkgs: {
      haskellPackages = pkgs.haskellPackages.override (oldArgs: {
        overrides = self: super:
                    let parent = (oldArgs.overrides or (_: _: {})) self super;
                    in parent // (import ./manual-overrides.nix { inherit self super pkgs lib local; });
      });
    })
  ];
in
  (import <nixpkgs> { inherit overlays; }).haskellPackages
