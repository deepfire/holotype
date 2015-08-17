let
  pkgs = (import <nixpkgs> {}).pkgs;
  ghc  = pkgs.haskellPackages;
  # ghc  = pkgs.haskell.packages.ghc;
  sdl2-newapi = pkgs.haskell.lib.overrideCabal ghc.sdl2 (oldAttrs: {
    buildDepends = [ ghc.linear ghc.text ghc.vector ];
    src          = pkgs.fetchgit {
      url    = https://github.com/deepfire/sdl2;
      rev    = "b1e4b1c4c504139c6bd444a6696f5c0dcaa16846";
      sha256 = "0w4q2fjrmanysk7v3p420799ph5k1ngjvwdb30ma57pkxlx5j7il";
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
