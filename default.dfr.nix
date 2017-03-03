{ mkDerivation, stdenv, src ? ./.
, aeson, attoparsec, base, base-unicode-symbols, binary, bytestring
, bytestring-trie, clock, containers, data-binary-ieee754, deepseq, digest
, directory, elerea, extra, filepath, free, GLFW-b, JuicyPixels
, microlens-platform, MissingH, MonadRandom, mono-traversable, OpenGLRaw
, pretty-show, process, proteaaudio, stm, time, transformers, vect, vector, zlib
#, youtrack
, lambdacube-compiler, lambdacube-gl, lambdacube-ir, mersenne-random-pure64
, lambdacube-quake3, force-layout, hxt, cairo, gi-cairo, gi-pango, gi-pangocairo, gi-gtk, gi-gdk
, wires, rapid
, ghc-typelits-extra, ghc-typelits-natnormalise
}:
mkDerivation {
  pname = "mood";
  version = "0.0.1";
  src = src;
  isLibrary = true;
  isExecutable = true;
  doHaddock = false;
  libraryHaskellDepends = [
    aeson attoparsec base base-unicode-symbols binary bytestring bytestring-trie clock containers
    data-binary-ieee754 deepseq digest directory extra filepath free JuicyPixels
    microlens-platform MissingH MonadRandom mono-traversable pretty-show process
    stm time transformers vect vector zlib
    # youtrack
    lambdacube-compiler lambdacube-gl lambdacube-ir mersenne-random-pure64
    lambdacube-quake3 force-layout hxt cairo gi-cairo gi-pango gi-pangocairo gi-gtk gi-gdk
    wires rapid
    ghc-typelits-extra ghc-typelits-natnormalise
  ];
  executableHaskellDepends = [
    base bytestring containers directory elerea filepath GLFW-b
    lambdacube-gl OpenGLRaw proteaaudio vect vector
  ];
  description  = "Visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
