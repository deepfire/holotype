{ nixpkgs     ? import <nixpkgs> {}
, compiler    ? import ./default-compiler.nix
, tools       ? false
, intero      ? tools
, local       ? false
}:
let
  pkgs    = nixpkgs.pkgs;
  ghc     = import ./packages.nix { inherit nixpkgs compiler local; }; # :: nixpkgs/pkgs/development/haskell-modules/make-package-set.nix
  extras  =  [
               ghc.ghc-events
               ghc.ghcid
               ghc.graphmod
               ghc.stylish-haskell
               pkgs.cabal-install
               pkgs.graphviz
             ] ++ (if intero then [ ghc.intero ] else []);
in
  ghc.shellFor {
    packages    = p: [p.holotype];
    withHoogle  = true;
    buildInputs = extras;
  }
