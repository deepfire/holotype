{ compiler    ? import ./default-compiler.nix
, local       ? false
, lib         ? import ./nix/lib.nix
, nixpkgs     ? lib.nixpkgs
, tools       ? false
}:
let
  pkgs     = import ./nixpkgs.nix { inherit compiler local nixpkgs; };   # Nixpkgs with overlays.
  ghc      = pkgs.haskell.packages."${compiler}";                        # :: nixpkgs/pkgs/development/haskell-modules/make-package-set.nix
  extras   = [
               ghc.ghc-events
               ghc.ghcid
               ghc.graphmod
               ghc.stylish-haskell
               pkgs.cabal-install
               pkgs.graphviz
             ];
in
  ghc.shellFor {
    packages    = p: [p.holotype];
    withHoogle  = true;
    buildInputs = extras;
  }
