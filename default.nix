# diagrams interpolate
# haskell-src-exts -> compdata -- Data types a la carte

with import <nixpkgs> {};
let
  default_compiler = "ghc7103";
  youtrack-src-github =
    pkgs.fetchgit {
      url    = https://github.com/deepfire/youtrack;
      rev    = "39eebdc7540183da75f97021a18418e9b67fbb77";
      sha256 = "0lc22d68zgpk38q67gm013pzi3d9n2imp7y2kfvsjcslxigygci9";
    };
  youtrack-src-local = ../youtrack;
  youtrack = hpkgs: src:
             with hpkgs;
             hpkgs.mkDerivation {
               pname = "youtrack";
               version = "0.0.6";
               src = src;
               libraryHaskellDepends = [
                 aeson base base-unicode-symbols bytestring HsOpenSSL http-client
                 http-client-openssl lens mtl parsers QuickCheck safe scientific split text time trifecta
                 unordered-containers utf8-string vector wreq
               ];
               homepage = "https://github.com/deepfire/youtrack";
               description = "Access a Jetbrains YouTrack instance";
               license = stdenv.lib.licenses.gpl3;
             };
in

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
