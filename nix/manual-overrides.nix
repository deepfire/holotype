{ self, super, pkgs, lib, local ? false }:

with pkgs.haskell.lib; with lib; with self; {

  # lambdacube-ir     = debugBuild super.lambdacube-ir;
  # reflex            = debugBuild super.reflex;
  # cairo             = debugBuild super.cairo;
  # gi-cairo          = debugBuild super.gi-cairo;
  # gi-pango          = debugBuild super.gi-pango;
  # gi-pangocairo     = debugBuild super.gi-pangocairo;
  # GLFW-b            = debugBuild super.GLFW-b;
  # GLURaw            = debugBuild super.GLURaw;
  # OpenGL            = debugBuild super.OpenGL;
  # OpenGLRaw         = debugBuild super.OpenGLRaw;
  # proteaaudio       = debugBuild super.proteaaudio;

  sop-core = mkDerivation {
    pname = "sop-core";
    version = "0.4.0.0";
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/generics-sop.src.json)) ["date"]);
    prePatch        = "cd sop-core; ";
    libraryHaskellDepends = [ base deepseq ];
    description = "True Sums of Products";
    license = pkgs.stdenv.lib.licenses.bsd3;
  };
  generic-lens = dontCheck super.generic-lens;
  generics-sop = overrideCabal super.generics-sop (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/generics-sop.src.json)) ["date"]);
    doCheck         = false;
    jailbreak       = true;
    editedCabalFile = null;
    revision        = null;
    prePatch        = "cd generics-sop; ";
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ sop-core ]);
  });

  reflex = overrideCabal super.reflex (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/reflex.src.json)) ["date"]);
    editedCabalFile = null;
    revision        = null;
    doCheck         = false;
    jailbreak       = true;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ lens data-default hlint filemanip monad-control monoidal-containers prim-uniq unbounded-delays MemoTrie ]);
  });
  reflex-glfw =
  mkDerivation {
    pname = "reflex-glfw";
    version = "0.1.0";
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/reflex-glfw.src.json)) ["date"]);
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

  ## Upstreamed, awaiting a Hackage release
  lambdacube-compiler = overrideCabal super.lambdacube-compiler (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/lambdacube-compiler.src.json)) ["date"]);
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ aeson semigroups exceptions megaparsec ansi-wl-pprint pretty-show lambdacube-ir vector ]);
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-gl = overrideCabal super.lambdacube-gl (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/lambdacube-gl.src.json)) ["date"]);
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-ir = overrideCabal super.lambdacube-ir (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/lambdacube-ir.src.json)) ["date"]);
    prePatch        = "cd lambdacube-ir.haskell; ";
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });

  iohk-monitoring = mkDerivation {
    pname = "iohk-monitoring";
    version = "0.1.0.0";
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./pins/iohk-monitoring-framework.src.json)) ["date"]);
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
  ekg = overrideCabal super.ekg (drv: {
    jailbreak       = true;
  });
  ekg-core = overrideCabal super.ekg-core (drv: {
    jailbreak       = true;
  });
  ekg-json = overrideCabal super.ekg-json (drv: {
    jailbreak       = true;
  });
}
