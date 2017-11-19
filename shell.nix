{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc802"
, tools       ? true
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
      # libearmap-category = overHackage old.libearmap-category "0.3.2.0" "011b4mjrl800vlyg1ibfmmyp87ad2mak6171s2mlc4mwsi4xrl4g" { doCheck = false; };
      intero              = overGithub (doJailbreak old.intero)
                            "commercialhaskell/intero"         "5697c86fde2b6131629e8d1c69f9b2363dadc7ae" "1zwvmchk8rymxfciiip78zf69p3f8jpbr7fqqj43cxv0lq9w284s" {};
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
      # bindings-GLFW       = doJailbreak old.bindings-GLFW;
      # gi-pango            = doJailbreak old.gi-pango;
      # gi-pangocairo       = doJailbreak (overGithub old.gi-pango
      #                       "gtk2hs/gtk2hs"                  "843d83548bd076975f1869e6768768b5ed24b268" "06hf9grra47z43zd6mp3q46rmqjl3n2i8w2g72j7x4frblww426m" {});
      # gtk2hs-buildtools   = doJailbreak old.gtk2hs-buildtools;
      # signal              = doJailbreak old.signal;
      # singletons          = doJailbreak (overGithub old.singletons
      #                       "goldfirere/singletons"          "d0fdb2cf02f29d6d076354696aaceb57f2715c85" "106iw4dsrgk6zsf49kbsiy3dg5q193bxihh3azxgf8gy48ymagck" {});
      text-lens           = doJailbreak old.text-lens;
      # these               = doJailbreak old.these;
      # th-desugar          = doJailbreak (overGithub old.th-desugar
      #                       "goldfirere/th-desugar"          "7649ccb1509da95f2086b5048bc17bda55473b49" "1062qq85cdf9vb35fls7w55lvwdjqdjanj1ha24z75j3ifa69d0r" {});
      # wavefront           = doJailbreak old.wavefront;
      # reflex              = doJailbreak (overGithub old.reflex
      #              	    "reflex-frp/reflex"              "72165498522201b4d339da7678922ef324fd28c6" "1zan8l2kczd4mnd1sd7sr1vjbyqrz9gm7bsy7jibjf8qb5asbgyp" {});
      reflex = doJailbreak (new.callPackage
      ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
       , exception-transformers, filemanip, haskell-src-exts, haskell-src-meta, hlint
       , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, semigroupoids, split, syb
       , template-haskell, these, transformers, transformers-compat, unbounded-delays
       }:
       mkDerivation {
           pname = "reflex";
           version = "0.5.0";
           src = pkgs.fetchFromGitHub {
             owner = "deepfire";
             repo = "reflex";
             rev = "60d2878142943487987feeeea108dbed5405f469";
             sha256 = "1gzyclfc18k881ww990fydgci3zcszy0rny0p979qdjfv4f50396";
           };
           libraryHaskellDepends = [
             base containers data-default dependent-map dependent-sum exception-transformers filemanip
             haskell-src-exts haskell-src-meta hlint lens MemoTrie monad-control mtl primitive prim-uniq ref-tf reflection
             semigroups semigroupoids split syb template-haskell these transformers transformers-compat unbounded-delays
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
                ++ (if intero then [ ghc.intero ] else []);
             });
in
  drv'.env
