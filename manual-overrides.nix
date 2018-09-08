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

}
