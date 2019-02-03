{ compiler       ? import ./nix/default-compiler.nix
, trace          ? false
, tracePatches   ? trace
, traceOverrides ? trace
}:
let
  pkgs     = import ./nix/nixpkgs.nix
    { inherit compiler trace tracePatches traceOverrides; }; # Nixpkgs with overlays.
  ghc      = pkgs.haskell.packages."${compiler}";            # :: nixpkgs/pkgs/development/haskell-modules/make-package-set.nix
  extras   = [
               ghc.ghc-events
               ghc.ghcid
               ghc.graphmod
               # ghc.cabal-install
               pkgs.graphviz
             ];
  localOnlyPackages = with ghc; with pkgs.lib; with builtins; {
  reflex-glfw = mkDerivation {
    pname = "reflex-glfw";
    version = "0.1.0";
    src = pkgs.fetchgit (removeAttrs (fromJSON (readFile ./local-pins/reflex-glfw.src-json)) ["date"]);
    isLibrary = true;
    isExecutable = true;
    jailbreak = true;
    libraryHaskellDepends = [
      base base-unicode-symbols containers dependent-sum GLFW-b lens mtl
      OpenGLRaw pretty primitive ref-tf reflex stm transformers
    ];
    executableHaskellDepends = [
      base base-unicode-symbols containers dependent-sum GLFW-b lens mtl
      OpenGL OpenGLRaw pretty reflex stm transformers
    ];
    homepage = "https://github.com/deepfire/reflex-glfw/";
    description = "A GLFW-b adapter for Reflex FRP";
    license = pkgs.stdenv.lib.licenses.bsd3;
  };
  iohk-monitoring = mkDerivation {
    pname = "iohk-monitoring";
    version = "0.1.0.0";
    src = pkgs.fetchgit (removeAttrs (fromJSON (readFile ./local-pins/iohk-monitoring-framework.src-json)) ["date"]);
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      aeson array async auto-update base bytestring clock containers
      contravariant directory ekg ekg-core exceptions filepath katip lens
      mtl safe-exceptions scientific stm template-haskell text time
      time-units transformers unix unordered-containers vector yaml
    ];
    executableHaskellDepends = [
      async base bytestring mtl random unix
    ];
    testHaskellDepends = [
      array async base bytestring clock containers mtl process QuickCheck
      random semigroups stm tasty tasty-hunit tasty-quickcheck text time
      time-units transformers unordered-containers void yaml
    ];
    description = "loggin, benchmarking and monitoring framework";
    license = stdenv.lib.licenses.mit;
  };
  };
in
  ghc.shellFor {
    packages    = p: [(ghc.callPackage (import ./default.nix) localOnlyPackages)];
    withHoogle  = true;
    buildInputs = extras;
  }
