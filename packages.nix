{ nixpkgs     ? import <nixpkgs> {}
, compiler    ? import ./default-compiler.nix
, local       ? false
}:

let
  pkgs = nixpkgs.pkgs;
  lib = {
    debugBuild = pkg: pkgs.haskell.lib.overrideCabal pkg (drv: {
      configureFlags  = "--ghc-option=-g --ghc-option=-O1";
      doHaddock       = false;
      dontStrip       = true;
    });
  };
  overlays = [
    (_: pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override (oldArgs: {
            overrides = self: super:
                        let parent = (oldArgs.overrides or (_: _: {})) self super;
                        in parent // import ./overrides.nix         { inherit self super pkgs lib; };
          });
        };
      };
    })
    (_: pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override (oldArgs: {
            overrides = self: super:
                        let parent = (oldArgs.overrides or (_: _: {})) self super;
                        in parent // import ./manual-overrides.nix { inherit self super pkgs lib; }
                        // {
                          holotype = self.callPackage (import ./default.nix) {};
                        };
          });
        };
      };
    })
  ];
in
  (import <nixpkgs> { inherit overlays; }).haskell.packages."${compiler}"
