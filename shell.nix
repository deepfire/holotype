{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc802"
, tools       ? true
, halive      ? tools
, intero      ? tools
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:
let
  localSrc      =      repo: rev: sha256:       pkgs.fetchgit { url = "file:///home/deepfire/src/" + repo; rev = rev; sha256 = sha256; };
  githubSrc     =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/"        + repo; rev = rev; sha256 = sha256; };
  overC         =                               pkgs.haskell.lib.overrideCabal;
  overCabal     = old:                    args: overC old (oldAttrs: (oldAttrs // args));
  overGithub    = old: repo: rev: sha256: args: overC old ({ src = githubSrc repo rev sha256; }    // args);
  overHackage   = old: version:   sha256: args: overC old ({ version = version; sha256 = sha256; } // args);
  overLocal     = old: repo: rev: sha256: args: overC old ({ src = localSrc repo rev sha256; }     // args);

  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      halive              = overGithub old.halive
                            "lukexi/halive" "2f1c4c4b00a2a046a2df21432456d7dd9c87ea7f" "0if5pdvkkxcyl2ybnvsmavg453l8c7is72lyy0i6c7d3hh3rcgnb" { doCheck = false; };
      # libearmap-category = overHackage old.libearmap-category "0.3.2.0" "011b4mjrl800vlyg1ibfmmyp87ad2mak6171s2mlc4mwsi4xrl4g" { doCheck = false; };
      lambdacube-compiler = overGithub (doJailbreak old.lambdacube-compiler)
                            "lambdacube3d/lambdacube-compiler" "132ccb3423c8c181bed8dc2219ad42091f485213" "16zhlbizvxxydb3wl829vqh4506lmxn2x0vvkq3sxj9k2c16gh5m" {};
      lambdacube-gl       = overGithub (doJailbreak old.lambdacube-gl)
                            "lambdacube3d/lambdacube-gl"       "297dfbf6f633a1269640b92ebc55b05ca58c102f" "13hd8cn3dpqz5mxrvf8a5rqq200icfq5lim73nzhzp8wqw6gw30h" {};
      # lambdacube-quake3   = overGithub (doJailbreak old.lambdacube-quake3)
      #                       "lambdacube3d/lambdacube-quake3"   "4ad4ed97c3596b411567da88653dcdd5ae0d7154" "1wy0b0cx37xn8f979rr3rcbh482xk8jh557npjh3nykpaa9687i6" {};
      lambdacube-ir       = doJailbreak old.lambdacube-ir;
      lambdacube-quake3 =
      new.mkDerivation {
        pname = "lambdacube-quake3";
        version = "0.1.0.0";
        isLibrary = true;
        isExecutable = true;
        doHaddock = false;
        doCheck = false;
        jailbreak = true;
        src = pkgs.fetchgit {
          url    = https://github.com/lambdacube3d/lambdacube-quake3;
          rev    = "4ad4ed97c3596b411567da88653dcdd5ae0d7154";
          sha256 = "1wy0b0cx37xn8f979rr3rcbh482xk8jh557npjh3nykpaa9687i6";
        };
        libraryHaskellDepends = [
          aeson attoparsec base binary bytestring bytestring-trie containers
          data-binary-ieee754 deepseq digest directory filepath JuicyPixels
          lambdacube-compiler lambdacube-gl lambdacube-ir mersenne-random-pure64
          microlens-platform MonadRandom pretty-show process
          time vect vector zlib
        ];
        executableHaskellDepends = [
          base bytestring containers directory filepath GLFW-b
          lambdacube-gl OpenGLRaw proteaaudio vect vector
        ];
        homepage = "lambdacube3d.com";
        description = "first person shooter game engine";
        license = "BSD";
      };
      number-show = callPackage
      ({ mkDerivation, base, microlens, microlens-th }:
       mkDerivation {
         pname = "number-show";
         version = "0.1.0.0";
         sha256 = "3e4ba74494fce27278ae6c07773d75be901be09c51028f47cbdc9087d9e29e06";
         revision = "1";
         editedCabalFile = "8c75cd93ac276a74f86693467c56de7ecb7c04f9d93d26c369199d7a7baaf173";
         libraryHaskellDepends = [ base microlens microlens-th ];
         description = "Flexible and accurate (for a given precision) numerical->string conversion";
         license = stdenv.lib.licenses.gpl3;
       }) {};
      text-lens           = doJailbreak old.text-lens;
      # reflex              = overGithub old.reflex
      #              	    "reflex-frp/reflex"              "e39bd393aec22b0ca28372040b68ef9d7c736826" "1l636aix0zbhnhzfqrq27vwiad70shjwk0vgkkczd76kka86ca16" {};
      reflex = doJailbreak (new.callPackage
      ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
       , exception-transformers, haskell-src-exts, haskell-src-meta, hlint
       , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, split, syb
       , template-haskell, these, transformers, transformers-compat
       }:
       mkDerivation {
           pname = "reflex";
           version = "0.5.0";
           src = pkgs.fetchFromGitHub {
             owner = "deepfire";
             repo = "reflex";
             rev = "e39bd393aec22b0ca28372040b68ef9d7c736826";
             sha256 = "1l636aix0zbhnhzfqrq27vwiad70shjwk0vgkkczd76kka86ca16";
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
       }) {});
    };
  });
  default = import ./.;
  drv     = ghc.callPackage default {};
  drv'    = haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                old.libraryHaskellDepends
                ++ [ pkgs.cabal-install pkgs.stack ]
                ++ (if intero then [ ghc.intero ] else [])
                ++ (if halive then [ ghc.halive ] else []);
             });
in
  drv'.env
