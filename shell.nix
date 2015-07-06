{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc7101"
}:
let
  pkgs = nixpkgs.pkgs;
  ghc  = pkgs.haskell.packages.${compiler};
  pkgf = import ./.;
  drv  = ghc.callPackage pkgf {};
  ghc-mod      = pkgs.haskell.lib.overrideCabal ghc.ghc-mod (oldAttrs: {
    src = pkgs.fetchgit {
      url = https://github.com/kazu-yamamoto/ghc-mod;
      rev = "247e4e0e7616fe1fecc68fdcf80d6249ac4cee4f";
      sha256 = "2a23271d0e6907351a246f095040ba18c3ab6bf1cba08a14338d701defa55474";
     };
    # the new ghc mod also requires some new dependencies. Add them to buildDepends:
    buildDepends = oldAttrs.buildDepends ++ [ cabal-helper ghc.cereal ];
  });
  cabal-helper = pkgs.haskell.lib.overrideCabal ghc.cabal-helper (oldAttrs: {
    version = "0.3.2.0";
    sha256 = "06igjmr0n8418wid1pr74cgvlsmwni7ar72g9bddivlbxax1pfli";
  });
in with pkgs;
  (pkgs.haskell.lib.addBuildTools drv [
    ghc.cabal-install
    ghc.halive
    ghc.hoogle-index
    ##
    cabal-helper
    ghc-mod
    ##
    emacs git ltrace silver-searcher strace
  ]).env
