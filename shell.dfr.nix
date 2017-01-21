{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc801"
, ghcOrig     ? pkgs.haskell.packages.${compiler}
}:
let
  hubsrc    =      repo: rev: sha256:        pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  overhub   = old: repo: rev: sha256: xargs: pkgs.haskell.lib.overrideCabal old (oldAttrs: { src = hubsrc repo rev sha256; }       // xargs);
  overcabal = old: version:   sha256: xargs: pkgs.haskell.lib.overrideCabal old (oldAttrs: { version = version; sha256 = sha256; } // xargs);

  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      halive = overhub   old.halive "lukexi/halive" "e9011910d326a5036f447e4f733235a86dd1987f" "149wdrm07b41d19wjw1hp2xhbb55a2x8ydz6nm9jl281wl8dc6xa" { doCheck = false; };
      elerea = overcabal old.elerea                                                    "2.8.0" "1sc71775f787dh70ay9fm6x6npsn81yci9yr984ai87ddz023sab" {};
      lambdacube-quake3 =
      new.mkDerivation {
        pname = "lambdacube-quake3";
        version = "0.1.0.0";
        isLibrary = true;
        isExecutable = true;
      	doHaddock = false;
      	doCheck = false;
        src = pkgs.fetchgit {
          url    = https://github.com/lambdacube3d/lambdacube-quake3;
          rev    = "79f8ff3dcb38c75c6e51e3994a317c77e9559255";
          sha256 = "0wsmrfzz8gm42sjrsxkl87kfg2z7acjxar79wn32z6lwz54z1nax";
        };
        libraryHaskellDepends = [
          aeson attoparsec base binary bytestring bytestring-trie containers
          data-binary-ieee754 deepseq digest directory filepath JuicyPixels
          lambdacube-compiler lambdacube-gl lambdacube-ir mersenne-random-pure64
          microlens-platform MonadRandom pretty-show process
          time vect vector zlib
        ];
        executableHaskellDepends = [
          base bytestring containers directory elerea filepath GLFW-b
          lambdacube-gl OpenGLRaw proteaaudio vect vector
        ];
        homepage = "lambdacube3d.com";
        description = "first person shooter game engine";
        license = "BSD";
      };
      youtrack =
      new.mkDerivation {
        pname = "youtrack";
        version = "0.0.6";
        src = youtrack-src-github;
        libraryHaskellDepends = [
          aeson base base-unicode-symbols bytestring HsOpenSSL http-client
          http-client-openssl lens mtl parsers QuickCheck safe scientific split text time trifecta
          unordered-containers utf8-string vector wreq
        ];
        homepage = "https://github.com/deepfire/youtrack";
        description = "Access a Jetbrains YouTrack instance";
        license = stdenv.lib.licenses.gpl3;
      };
    };
  });
  
  youtrack-src-github = pkgs.fetchgit {
    url    = https://github.com/deepfire/youtrack;
    rev    = "39eebdc7540183da75f97021a18418e9b67fbb77";
    sha256 = "0lc22d68zgpk38q67gm013pzi3d9n2imp7y2kfvsjcslxigygci9";
  };
  #
  youtrack-src-local  = ../youtrack;
in

(haskell.lib.addBuildTools
  (ghc.callPackage (import ./.) { })
  [ pkgs.cabal-install
    pkgs.stack
    ghc.halive
    ghc.intero
  ]).env
