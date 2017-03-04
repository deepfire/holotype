{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc801"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:
let
  overcabal = pkgs.haskell.lib.overrideCabal;
  hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);

  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      halive = overhub   old.halive "lukexi/halive" "2f1c4c4b00a2a046a2df21432456d7dd9c87ea7f" "0if5pdvkkxcyl2ybnvsmavg453l8c7is72lyy0i6c7d3hh3rcgnb" { doCheck = false; };
      haskell-gi = old.haskell-gi_0_20;
      haskell-gi-base = old.haskell-gi-base_0_20;
      gi-atk = old.gi-atk_2_0_11;
      gi-gobject = old.gi-gobject_2_0_11;
      gi-cairo = old.gi-cairo_1_0_11;
      gi-gdk = old.gi-gdk_3_0_11;
      gi-gio = old.gi-gio_2_0_11;
      gi-gdkpixbuf = old.gi-gdkpixbuf_2_0_11;
      gi-glib = old.gi-glib_2_0_11;
      gi-gtk = old.gi-gtk_3_0_11;
      gi-pango = old.gi-pango_1_0_11;
      gi-pangocairo = overrideCabal (old.gi-pangocairo.overrideScope (self: super: {
      })) (old: with pkgs; {
        libraryPkgconfigDepends = [ pango.dev cairo gobjectIntrospection ];
        preConfigure = ''export HASKELL_GI_GIR_SEARCH_PATH=${pango.dev}/share/gir-1.0'';
        preCompileBuildDriver = ''
          PKG_CONFIG_PATH+=":${pango.dev}/lib/pkgconfig:${cairo}/lib/pkgconfig"
          setupCompileFlags+=" $(pkg-config --libs pangocairo cairo-gobject)"
        '';
      });
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
      reflex = new.callPackage
         ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
          , exception-transformers, haskell-src-exts, haskell-src-meta, hlint
          , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, split, syb
          , template-haskell, these, transformers, transformers-compat
          }:
          mkDerivation {
            pname = "reflex";
            version = "0.5.0";
	    src = pkgs.fetchFromGitHub {
	      owner = "reflex-frp";
	      repo = "reflex";
              rev = "d78ba4318c425ca9b942dc387d7c5c7ab2d2e095";
              sha256 = "10sryvwdf88ajkp35yma8llkb38cp63vjr5mq2hba4s2d8yg649q";
            };
            libraryHaskellDepends = [
              base containers data-default dependent-map dependent-sum exception-transformers
              haskell-src-exts haskell-src-meta hlint lens MemoTrie monad-control mtl primitive prim-uniq ref-tf reflection
	      semigroups split syb template-haskell these transformers transformers-compat
            ];
            testHaskellDepends = [
              base containers dependent-map MemoTrie mtl ref-tf
            ];
            homepage = "https://github.com/reflex-frp/reflex";
            description = "Higher-order Functional Reactive Programming";
            license = stdenv.lib.licenses.bsd3;
            hydraPlatforms = stdenv.lib.platforms.none;
          }) {};
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
