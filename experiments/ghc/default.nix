{ mkDerivation, stdenv, base, cabal-install, generics-sop
}:
mkDerivation {
  pname                 = "holotype";
  version               = "0.0.1";
  src                   = ./.;
  isLibrary             = true;
  isExecutable          = true;
  libraryHaskellDepends = [
    base
    cabal-install
    generics-sop
  ];
  description           = "Graph-backed visual mind assistant";
  license               = stdenv.lib.licenses.agpl3;
  doHaddock             = false;
}
