# diagrams interpolate
# haskell-src-exts -> compdata -- Data types a la carte
 { stdenv, mkDerivation, authinfo-hs, base, base-unicode-symbols, cassava, compdata, containers, hscolour, http-client, http-client-openssl, linear, netwire, optparse-generic, pkgconfig, reflection, sdl2, time, vector, vty, wreq, trifecta, parsers, json-schema, json-autotype }:
mkDerivation {
  pname        = "mood";
  version      = "0.0.1";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
  buildDepends =       [ authinfo-hs  base  base-unicode-symbols  cassava  compdata  containers  hscolour  http-client  http-client-openssl  linear  netwire  optparse-generic  pkgconfig  reflection  sdl2  vector  vty  wreq  trifecta  time  parsers  json-schema  json-autotype ];
  description  = "Visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
