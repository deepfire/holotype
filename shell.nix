{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, tools       ? false
, intero      ? tools
, local       ? false
}:
let
  ghc     = import ./ghc.nix      { inherit nixpkgs pkgs haskell compiler ghcOrig local; };
  drv     = import ./package.nix  { inherit nixpkgs pkgs haskell compiler ghcOrig local; };
  drv'    = haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                old.libraryHaskellDepends
                ++ [ pkgs.cabal-install pkgs.stack ghc.ghc-events ]
                ++ (if intero then [ ghc.intero ] else []);
             });
in
  drv'.env
