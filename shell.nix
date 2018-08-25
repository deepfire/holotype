{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, tools       ? false
, intero      ? tools
, local       ? false
}:
let
  drv     = import ./package.nix  { inherit nixpkgs pkgs compiler ghcOrig local; };
  ghc     = import ./ghc.nix      { inherit nixpkgs pkgs compiler ghcOrig local; };
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                old.libraryHaskellDepends
                ++ [ pkgs.cabal-install pkgs.stack ghc.ghc-events ghc.graphmod pkgs.graphviz ]
                ++ (if intero then [ ghc.intero ] else []);
              libraryPkgconfigDepends = [ pkgs.cairo pkgs.pango ];
              doHaddock = false;
              preCompileBuildDriver = ''
                PKG_CONFIG_PATH+=":${pkgs.cairo}/lib/pkgconfig"
                setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
              '';
             });
in
  drv'.env
