let pkgs =  (import <nixpkgs> {}).pkgs;
    sdl2-newapi = pkgs.haskell-ng.lib.overrideCabal pkgs.haskellngPackages.sdl2 (oldAttrs: {
      src = pkgs.fetchgit {
        url = https://github.com/haskell-game/sdl2;
        rev = "5c406b34c513615cbd3da1bb8ef60c1f465925c6";
        sha256 = "13q9ic6d60qvbaqqcrrs1warj6nd30b38n4pm283j0dszv17vzkr";
       };
      buildDepends = oldAttrs.buildDepends ++ [ pkgs.haskellPackages.linear pkgs.haskellPackages.text pkgs.haskellPackages.vector ];
    });
    ghc-mod-git = pkgs.haskell-ng.lib.overrideCabal pkgs.haskellngPackages.ghc-mod (oldAttrs: {
      src = pkgs.fetchgit {
        url = https://github.com/kazu-yamamoto/ghc-mod;
        rev = "247e4e0e7616fe1fecc68fdcf80d6249ac4cee4f";
        sha256 = "2a23271d0e6907351a246f095040ba18c3ab6bf1cba08a14338d701defa55474";
       };
      # the new ghc mod also requires some new dependencies. Add them to buildDepends:
      buildDepends = oldAttrs.buildDepends ++ [ cabal-helper-new pkgs.haskellngPackages.cereal ];
    });
    cabal-helper-new = pkgs.haskell-ng.lib.overrideCabal pkgs.haskellngPackages.cabal-helper (oldAttrs: {
      version = "0.3.2.0";
      sha256 = "06igjmr0n8418wid1pr74cgvlsmwni7ar72g9bddivlbxax1pfli";
    });
  in
with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, pkgconfig, containers, netwire, sdl2, stdenv, linear, vector, hoogle-index, halive, diagrams }:
             mkDerivation {
               pname = "mood";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base containers netwire sdl2-newapi strace ltrace
                                emacs git silver-searcher haskellngPackages.cabal-install cabal-helper-new ghc-mod-git
                                linear vector pkgconfig hoogle-index halive diagrams
                              ];
               homepage = "https://github.com/_deepfire/mood";
               description = "A mind-assisting graph substrate";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
