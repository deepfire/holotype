{ nixpkgs     ? import <nixpkgs> {}
, compiler    ? import ./default-compiler.nix
, tools       ? false
, intero      ? tools
, local       ? false
}:
let
  pkgs    = nixpkgs.pkgs;
  drv     = import ./package.nix  { inherit nixpkgs compiler local; };
  ghc     = import ./ghc.nix      { inherit nixpkgs compiler local; };
  extras  =  [
               pkgs.cabal-install
               ghc.ghc-events
               ghc.graphmod
               pkgs.graphviz
             ] ++ (if intero then [ ghc.intero ] else []);
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends   = old.libraryHaskellDepends ++ extras;
              libraryPkgconfigDepends = [ pkgs.cairo pkgs.pango ];
              doHaddock               = false;
              preCompileBuildDriver   = ''
                PKG_CONFIG_PATH+=":${pkgs.cairo}/lib/pkgconfig"
                setupCompileFlags+=" $(pkg-config --libs cairo-gobject)"
              '';
             });
in
  drv'.env
