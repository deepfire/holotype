let
  nixpkgsJson  = ./pins/default-nixpkgs-src.json;
  fetchNixpkgs = import ./fetch-nixpkgs.nix;
  nixpkgs      = fetchNixpkgs nixpkgsJson;
  debugBuild   = pkg: nixpkgs.haskell.lib.overrideCabal pkg (drv: {
    configureFlags  = "--ghc-option=-g --ghc-option=-O1";
    doHaddock       = false;
    dontStrip       = true;
  });
in {
  inherit nixpkgsJson fetchNixpkgs nixpkgs;
  inherit debugBuild;
}
