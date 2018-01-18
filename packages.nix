{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc841"
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
                rev = "161fe1cef15224c7ed2e9916b335eac9f483e612";
                sha256 = "1qh4092bfdz1cr0n5jf34xgxmy3l4j4jqprmddkhvpw492qlcczp";
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
