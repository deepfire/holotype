{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
, from-github ? false
}:
let
  pkgs = nixpkgs.pkgs;
  haskell = pkgs.haskell;
  ghcOrig = haskell.packages.${compiler};

  ## hois
  srchois-github =
    pkgs.fetchgit {
      url    = https://github.com/deepfire/hois;
      rev    = "0130d8be118cd841813d47e048bc97666fa5892d";
      sha256 = "03g8wgd0sg1yb60gxr7wga7bnb1h9qzaphrpj1gi1a4fgcgny7mm";
    };
  srchois-local = ../hois;
  srchois = if from-github then srchois-github else srchois-local;

  ## cgen
  srccgen-github =
    pkgs.fetchgit {
      url    = https://github.com/deepfire/cgen;
      rev    = "b087ce1c3f53381a6d42f0fa7924fadf22f42f47";
      sha256 = "126lkq76f9s0yviczkl4d8rvxhqj15dlp5mmnbff673d4cabz84v";
    };
  srccgen-local = ../cgen;
  srccgen = if from-github then srccgen-github else srccgen-local;

  ## hogre
  srchogre-github =
    pkgs.fetchgit {
      url    = https://github.com/deepfire/hogre;
      rev    = "0130d8be118cd841813d47e048bc97666fa5892a";
      sha256 = "03g8wgd0sg1yb60gxr7wga7bnb1h9qzaphrpj1gi1a4fgcgny7ma";
    };
  srchogre-local = ../hogre;
  srchogre = if from-github then srchogre-github else srchogre-local;

  ghc     = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
      hois  = haskell.lib.overrideCabal old.hois  (oldAttrs: { src = srchois; });
      cgen  = haskell.lib.overrideCabal old.cgen  (oldAttrs: { src = srccgen; });
      hogre = haskell.lib.overrideCabal old.hogre (oldAttrs: { src = srchogre; });
    };
  });
  pkgf = import ./.;
  drv  = ghc.callPackage pkgf {
    # hoogle = callPackage ./hoogle.nix { }
  };
in with pkgs;
  (haskell.lib.addBuildTools drv [
    # ghc.cabal-install
    # ghc.halive
    # ghc.hoogle-index
    ##
    # emacs git ltrace silver-searcher strace
  ]).env
