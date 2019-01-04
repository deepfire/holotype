{ compiler    ? import ./default-compiler.nix
, local       ? false
, lib         ? import ./lib.nix
, nixpkgs     ? lib.nixpkgs
}:

let
  import ./nixpkgs.nix { inherit compiler local nixpkgs; } # :: nixpkgs/pkgs/development/haskell-modules/make-package-set.nix