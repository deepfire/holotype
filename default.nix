let
  pkgs = (import <nixpkgs> {}).pkgs;
  ghc  = pkgs.haskellPackages;
  # ghc  = pkgs.haskell.packages.ghc;
  sdl2-newapi = pkgs.haskell.lib.overrideCabal ghc.sdl2 (oldAttrs: {
    buildDepends = [ ghc.linear ghc.text ghc.vector ];
    src          = pkgs.fetchgit {
      url    = https://github.com/deepfire/sdl2;
      rev    = "bb2c4b6b52b48497f3271fc880dd1a0b11623ef7";
      sha256 = "0fcirg3g2kd9d001hzz60mililp79l36prh0nqpcw0fzk5zzp9y2";
     };
  });
in
{ mkDerivation, base, base-unicode-symbols, containers, diagrams, linear, netwire, pkgconfig, reflection, stdenv, vector }:
mkDerivation {
  pname        = "mood";
  version      = "0.0.1";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
  buildDepends = [ base base-unicode-symbols containers diagrams linear netwire pkgconfig reflection sdl2-newapi vector
                 ];
  description  = "Graph-backed visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
