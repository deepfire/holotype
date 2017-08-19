{ mkDerivation, aeson, base, base-unicode-symbols, binary
, bytestring, cairo, clock, containers, dependent-sum, directory
, dlist, exceptions, extra, filepath, free, ghc-prim
, ghc-typelits-extra, ghc-typelits-natnormalise, gi-cairo, gi-pango
, gi-pangocairo, GLFW-b, hashable, haskell-gi-base, hspec
, hxt, JuicyPixels, lambdacube-compiler, lambdacube-gl
, lambdacube-ir, lambdacube-quake3, lens, linear, lub, metamorphic
, MissingH, MonadRandom, mono-traversable, mtl, OpenGL, OpenGLRaw
, pretty, pretty-show, profunctors, proteaaudio, QuickCheck, random
, rapid, reflex, semigroupoids, semigroups, stdenv, stm, text
, text-lens, text-zipper, these, transformers, tuple
, unordered-containers, vect, vector
}:
mkDerivation {
  pname = "holotype";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base-unicode-symbols binary bytestring cairo clock
    containers dependent-sum directory dlist exceptions extra filepath
    free ghc-prim ghc-typelits-extra ghc-typelits-natnormalise gi-cairo
    gi-pango gi-pangocairo GLFW-b hashable haskell-gi-base hspec
    hxt JuicyPixels lambdacube-compiler lambdacube-gl lambdacube-ir
    lambdacube-quake3 lens linear lub metamorphic MissingH MonadRandom
    mono-traversable mtl OpenGL OpenGLRaw pretty pretty-show
    profunctors proteaaudio QuickCheck random rapid reflex
    semigroupoids semigroups stm text text-lens text-zipper these
    transformers tuple unordered-containers vect vector
  ];
  executableHaskellDepends = [
    aeson base base-unicode-symbols bytestring cairo clock containers
    directory free gi-cairo gi-pangocairo GLFW-b haskell-gi-base
    lambdacube-compiler lambdacube-gl lambdacube-ir lambdacube-quake3
    lens linear OpenGLRaw pretty-show text text-zipper vector
  ];
  description = "Graph-backed visual mind assistant";
  license = stdenv.lib.licenses.agpl3;
}
