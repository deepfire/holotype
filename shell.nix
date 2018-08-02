{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, tools       ? false
, intero      ? tools
}:
let

  ghc     = import ./packages.nix { inherit nixpkgs pkgs haskell compiler ghcOrig; };
  default = import ./.;
  drv     = ghc.callPackage default {};
  drv'    = haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                old.libraryHaskellDepends
                ++ [ pkgs.cabal-install pkgs.stack ]
                ++ (if intero then [ ghc.intero ] else []);
             });
in
  drv'.env
