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
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./generics-sop.src.json)) ["date"]);
    prePatch        = "cd sop-core; ";
    libraryHaskellDepends = [ base deepseq ];
    description = "True Sums of Products";
    license = stdenv.lib.licenses.bsd3;
  };
  generics-sop = overrideCabal super.generics-sop (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./generics-sop.src.json)) ["date"]);
    doCheck         = false;
    jailbreak       = true;
    editedCabalFile = null;
    revision        = null;
    prePatch        = "cd generics-sop; ";
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ sop-core ]);
  });

  reflex-glfw =
  mkDerivation {
    pname = "reflex-glfw";
    version = "0.1.0";
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./reflex-glfw.src.json)) ["date"]);
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
    license = stdenv.lib.licenses.bsd3;
  };

  ## Upstreamed, awaiting a Hackage release
  lambdacube-compiler = overrideCabal super.lambdacube-compiler (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-compiler.src.json)) ["date"]);
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ aeson semigroups exceptions megaparsec ansi-wl-pprint pretty-show lambdacube-ir vector ]);
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-gl = overrideCabal super.lambdacube-gl (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-gl.src.json)) ["date"]);
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-ir = overrideCabal super.lambdacube-ir (drv: {
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-ir.src.json)) ["date"]);
    prePatch        = "cd lambdacube-ir.haskell; ";
    jailbreak       = true;
    # enableLibraryProfiling = false;
  });
}
