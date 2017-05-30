{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc802"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:
let
  overcabal = pkgs.haskell.lib.overrideCabal;
  hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  locsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "file:///home/deepfire/src/" + repo; rev = rev; sha256 = sha256; };
  overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);
  overloc   = old: repo: rev: sha256: args: overc old ({ src = locsrc repo rev sha256; }       // args);

  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      halive         = overhub  old.halive "lukexi/halive" "2f1c4c4b00a2a046a2df21432456d7dd9c87ea7f" "0if5pdvkkxcyl2ybnvsmavg453l8c7is72lyy0i6c7d3hh3rcgnb" { doCheck = false; };
      # libearmap-category = overhage old.libearmap-category "0.3.2.0" "011b4mjrl800vlyg1ibfmmyp87ad2mak6171s2mlc4mwsi4xrl4g" { doCheck = false; };
      # lambdacube-compiler = doJailbreak old.lambdacube-compiler;
      lambdacube-compiler = overhub (doJailbreak old.lambdacube-compiler)
                            "deepfire/lambdacube-compiler" "b3642f2d41b57d24b485216487d3c9578c52bce5" "1s4jr81p5n3arzjgwgxdmr5006s1dcba3xj46nwnn8yzq8s7iwnh" {};
      lambdacube-gl       = overhub (doJailbreak old.lambdacube-gl)
                            "lambdacube3d/lambdacube-gl"   "51d36d68d94e3d5053dcda2ee072fe11893b727d" "1l0i5nmqf6ypv3pkvky70yw3hkwm24m97bbr0aapzdldqjqc6f56" {};
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
      # reflex              = overhub old.reflex
      #              	    "deepfire/reflex"              "ca928573e6d1a17fe02de2d89d410db8f24d34e8" "1lmgfiz76cv5bh8ri9v7k2djzjd9rmm8lhgw8kd9x7hh9331f15a" {};
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
             owner = "deepfire";
             repo = "reflex";
             rev = "ca928573e6d1a17fe02de2d89d410db8f24d34e8";
             sha256 = "1lmgfiz76cv5bh8ri9v7k2djzjd9rmm8lhgw8kd9x7hh9331f15a";
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
    };
  });
in

(haskell.lib.addBuildTools
  (ghc.callPackage (import ./.) { })
  [ pkgs.cabal-install
    pkgs.stack
    ghc.halive
    ghc.intero
  ]).env
