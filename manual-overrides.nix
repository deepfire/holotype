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

  # lambdacube-gl = overrideCabal (debugBuild super.lambdacube-gl) (drv: {
  #   src = if local
  #         then
  #         builtins.filterSource (path: type:
  #           type != "unknown"           &&
  #           baseNameOf path != ".git"   &&
  #           baseNameOf path != "result" &&
  #           baseNameOf path != "dist") ../lambdacube-gl
  #         else
  #         pkgs.fetchFromGitHub {
  #           owner  = "lambdacube3d";
  #           repo   = "lambdacube-gl";
  #           rev    = "297828bdcf105c5942ed0e43d9f28130f543f34c";
  #           sha256 = "1gclb1wn5rl23vsrl1zs3lhiyyddrga6kggrnkpsyi8bwgq8l5z7";
  #         };
  # });

  lambdacube-quake3 = super.mkDerivation {
    configureFlags  = "--ghc-option=-g --ghc-option=-O1";
    doHaddock       = false;
    dontStrip       = true;

    pname = "lambdacube-quake3";
    version = "0.1.0.0";
    src = pkgs.fetchFromGitHub {
      owner  = "deepfire";
      repo   = "lambdacube-quake3";
      rev    = "4931ffc0459847d3cefc8f6ab0215a6066234c06";
      sha256 = "0ri1nf3vk2gaiwj2kbfa33lf5qammxj7d9c2x0wim8jvp94svxlv";
    };
    isLibrary = true;
    isExecutable = true;
    enableSeparateDataOutput = true;
    libraryHaskellDepends = [
      aeson base binary bytestring containers data-binary-ieee754 deepseq
      digest directory filepath hashable JuicyPixels lambdacube-compiler
      lambdacube-gl lambdacube-ir megaparsec mersenne-random-pure64
      microlens-platform MonadRandom mtl pretty-show process time
      transformers unordered-containers vect vector zlib
    ];
    executableHaskellDepends = [
      base binary bytestring containers deepseq digest directory elerea
      filepath GLFW-b JuicyPixels lambdacube-gl mersenne-random-pure64
      microlens-platform MonadRandom mtl OpenGLRaw pretty-show
      proteaaudio transformers unordered-containers vect vector
    ];
    homepage = "lambdacube3d.com";
    description = "first person shooter game engine";
    license = pkgs.stdenv.lib.licenses.bsd3;
  };
}
