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
              license = stdenv.lib.licenses.bsd3;
            };
          };
  })
