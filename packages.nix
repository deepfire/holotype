{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc843"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:

ghcOrig.override (oldArgs: {
    overrides = new: old:
    with new; ((oldArgs.overrides or (_: _: {})) new old)
       // import ./overrides.nix { inherit pkgs; self = new; super = old; haskellLib = haskell.lib; }
       // {
            lambdacube-quake3 =
            new.mkDerivation {
              pname = "lambdacube-quake3";
              version = "0.1.0.0";
              src = pkgs.fetchFromGitHub {
                owner = "deepfire";
                repo = "lambdacube-quake3";
                rev = "734b141eeca2722af7dff886318606c3f8ad54be";
                sha256 = "1l7q866mxl9lcj7nryjs08vfgy26a8d4lpazi3x74nkw7i21zigh";
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
              license = stdenv.lib.licenses.bsd3;
            };
          };
  })
