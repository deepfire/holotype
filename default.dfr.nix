{ mkDerivation, stdenv, src ? ./.
, aeson, attoparsec, base, base-unicode-symbols, binary, bytestring
, bytestring-trie, containers, data-binary-ieee754, deepseq, digest
, directory, elerea, filepath, GLFW-b, JuicyPixels
, microlens-platform, MissingH, MonadRandom, OpenGLRaw
, pretty-show, process, proteaaudio, time, vect, vector, zlib
#, youtrack
, lambdacube-compiler, lambdacube-gl, lambdacube-ir, mersenne-random-pure64
, lambdacube-quake3, force-layout, hxt, gi-cairo, gi-pango, gi-pangocairo
}:
mkDerivation {
  pname = "mood";
  version = "0.0.1";
  src = src;
  isLibrary = true;
  isExecutable = true;
  doHaddock = false;
  libraryHaskellDepends = [
    aeson attoparsec base base-unicode-symbols binary bytestring bytestring-trie containers
    data-binary-ieee754 deepseq digest directory filepath JuicyPixels
    microlens-platform MissingH MonadRandom pretty-show process
    time vect vector zlib
    # youtrack
    lambdacube-compiler lambdacube-gl lambdacube-ir mersenne-random-pure64
    lambdacube-quake3 force-layout hxt gi-cairo gi-pango gi-pangocairo
  ];
  executableHaskellDepends = [
    base bytestring containers directory elerea filepath GLFW-b
    lambdacube-gl OpenGLRaw proteaaudio vect vector
  ];
  description  = "Visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
