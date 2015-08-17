let
  pkgs = (import <nixpkgs> {}).pkgs;
  ghc  = pkgs.haskellPackages;
  # ghc  = pkgs.haskell.packages.ghc;
  sdl2-newapi = pkgs.haskell.lib.overrideCabal ghc.sdl2 (oldAttrs: {
    buildDepends = [ ghc.linear ghc.text ghc.vector ];
    src          = pkgs.fetchgit {
      url    = https://github.com/haskell-game/sdl2;
      rev    = "5c406b34c513615cbd3da1bb8ef60c1f465925c6";
      sha256 = "13q9ic6d60qvbaqqcrrs1warj6nd30b38n4pm283j0dszv17vzkr";
     };
  });
in
{ mkDerivation, base, base-unicode-symbols, containers, diagrams, linear, netwire, pkgconfig, stdenv, vector }:
mkDerivation {
  pname        = "mood";
  version      = "0.0.1";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
  buildDepends = [ base base-unicode-symbols containers diagrams linear netwire pkgconfig sdl2-newapi vector
                 ];
  description  = "Graph-backed visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
