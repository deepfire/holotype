{ mkDerivation, aeson, aeson-pretty, base, base-unicode-symbols
, binary, bytestring, cairo, clock, containers, data-default
, deepseq, dependent-sum, directory, dlist, exceptions, extra
, fclabels, filepath, free, fused-effects, generics-sop, ghc-prim
, gi-cairo, gi-gobject, gi-pango, gi-pangocairo, GLFW-b, Glob
, hashable, haskell-gi-base, hedgehog, hspec, hxt, iohk-monitoring
, JuicyPixels, lambdacube-compiler, lambdacube-gl, lambdacube-ir
, lens, lens-sop, linear, lub, metamorphic, MissingH, monadplus
, MonadRandom, mono-traversable, monoidal-containers, mtl
, newtype-generics, OpenGL, OpenGLRaw, optparse-applicative
, parsers, pretty, pretty-show, prettyprinter, primitive
, profunctors, QuickCheck, random, ref-tf, reflex, reflex-glfw
, safe, semigroupoids, semigroups, singletons, sop-core, spool
, stdenv, stm, tasty, tasty-discover, tasty-expected-failure
, tasty-hedgehog, tasty-hspec, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, template-haskell, text, text-format, text-lens
, text-zipper, these, time, transformers, trifecta, type-map
, unordered-containers, vect, vector
}:
mkDerivation {
  pname = "holotype";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base base-unicode-symbols binary bytestring
    cairo clock containers data-default deepseq dependent-sum directory
    dlist exceptions extra fclabels filepath free fused-effects
    generics-sop ghc-prim gi-cairo gi-gobject gi-pango gi-pangocairo
    GLFW-b hashable haskell-gi-base hedgehog hspec hxt iohk-monitoring
    JuicyPixels lambdacube-compiler lambdacube-gl lambdacube-ir lens
    lens-sop linear lub metamorphic MissingH monadplus MonadRandom
    mono-traversable monoidal-containers mtl newtype-generics OpenGL
    OpenGLRaw optparse-applicative parsers pretty pretty-show
    prettyprinter primitive profunctors QuickCheck random ref-tf reflex
    reflex-glfw safe semigroupoids semigroups singletons sop-core spool
    stm tasty tasty-expected-failure tasty-hedgehog tasty-hspec
    tasty-hunit tasty-quickcheck template-haskell text text-format
    text-lens text-zipper these time transformers trifecta type-map
    unordered-containers vect vector
  ];
  executableHaskellDepends = [
    aeson base base-unicode-symbols bytestring cairo clock containers
    directory free generics-sop gi-cairo gi-gobject gi-pango
    gi-pangocairo GLFW-b haskell-gi-base lambdacube-compiler
    lambdacube-gl lambdacube-ir lens linear mtl OpenGLRaw pretty-show
    ref-tf reflex reflex-glfw text text-zipper time vector
  ];
  testHaskellDepends = [
    base base-unicode-symbols containers directory filepath Glob
    hedgehog lens linear tasty tasty-discover tasty-expected-failure
    tasty-hedgehog tasty-hspec tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  testToolDepends = [ tasty-discover ];
  description = "Graph-backed visual mind assistant";
  license = stdenv.lib.licenses.agpl3;
}
