{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
}:
let
  pkgs = nixpkgs.pkgs;
  haskell = pkgs.haskell;
  ghcOrig = haskell.packages.${compiler};
  ghc      = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
      HTTP           =           (doJailbreak old.HTTP);
      bifunctors     = dontCheck (doJailbreak old.bifunctors);
      comonad        = dontCheck (doJailbreak old.comonad);
      aeson          = dontCheck (haskell.lib.overrideCabal old.aeson (oldAttrs: {
        buildDepends = [ new.fail new.tagged ];
	editedCabalFile = null;
	revision        = null;
        src = pkgs.fetchgit {
                url    = https://github.com/bos/aeson;
                rev    = "d9f1ad357e5dc28c381099814e7cce0e68cf2b6a";
                sha256 = "0nzajw4ia2s7kjjygw9xddrfa1aq2kq960bm9wgj8b34manmyh0f";
        };
      }));
      aeson-utils      = (doJailbreak old.aeson-utils);
      authenticate-oauth = dontCheck (doJailbreak old.authenticate-oauth);
      doctest        = dontCheck (haskell.lib.overrideCabal old.doctest (oldAttrs: {
        buildDepends = [ new.base-compat ];
        src = pkgs.fetchgit {
                url    = https://github.com/deepfire/doctest;
                rev    = "167fa799be1f642d388c750b03a188d9096e03a2";
                sha256 = "06myafhfhgmvrcfgi7vp27cnwg83119pdjd9hsc03kz1hd0pqwv7";
        };
      }));
      fail           = dontHaddock (dontCheck (doJailbreak old.fail));
      generic-aeson  = doJailbreak (haskell.lib.overrideCabal old.generic-aeson (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/silkapp/generic-aeson/;
                rev    = "f85e6a805d296a8037d0c21828a9e4185d490910";
                sha256 = "1c717c05z3nk1q9wgnxg6pxsywjf1d95r4v9509b1b8vvj67gfkv";
        };
      }));
      haskell-src-exts        = dontCheck (haskell.lib.overrideCabal old.haskell-src-exts (oldAttrs: {
        src = pkgs.fetchgit {
                # url    = https://github.com/mpickering/haskell-src-exts;
                # rev    = "49faf5996dec083b8f33fac23de8f56493e93df3";
                # sha256 = "1fj924hrpys8mngncbh6ql1j9a766xsj4wppfy9484jyi3p00cga";
                url    = https://github.com/haskell-suite/haskell-src-exts;
                rev    = "291cc802fda331e7fb69c57be77fc95aac7fb18c";
                sha256 = "05i4a0nsd4cv8waz5j4d2fr5a6kk23v2qfxc4y7vws1xgq6f71jb";
        };
      }));
      hflags        = haskell.lib.overrideCabal old.hflags (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/deepfire/hflags;
                rev    = "f4090a1131b7cb3ca0078fee2dc797ed8453f41a";
                sha256 = "1a3fl41sr34iz6cnygvngl2f88xxbpn5jhan6rqcxqsda86h6ri8";
        };
      });
      http-date      = (dontCheck old.http-date);
      json-autotype  = (doJailbreak old.json-autotype);
      json-schema    = (doJailbreak old.json-schema);
      kan-extensions = dontCheck (haskell.lib.overrideCabal old.kan-extensions (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/ekmett/kan-extensions.git;
                rev    = "99df306a69f91f6c36ac3e98a5f4a31b7c7ba6f4";
                sha256 = "0vd3z37a0bfsgkmisr917gd65g3jix4xpb11xyyyfl3xyac447gl";
        };
      }));
      lens           = dontCheck (haskell.lib.overrideCabal old.lens (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/ekmett/lens.git;
                rev    = "64cce394ae9b1ee668892906beab15a97c900862";
                sha256 = "17j93h5c63psyc2y5wvhp89nighypskykgix3bw0l4kl6h5i18a3";
        };
      }));
      lens-aeson     = dontCheck (doJailbreak old.lens-aeson);
      linear         = dontCheck (haskell.lib.overrideCabal old.linear (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/ekmett/linear.git;
                rev    = "7de2733b1d922a2717860df49b6090042b81ea35";
                sha256 = "0c3cw4b9cnypi1rjdyzp5xb0qy088q3mg5in8q5lcvqfmpzfc22b";
        };
      }));
      parsers        = doJailbreak old.parsers;
      psqueues       = dontCheck (haskell.lib.overrideCabal old.psqueues (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/bttr/psqueues.git;
                rev    = "51a4721daec536c6c63d66958b893abe563df528";
                sha256 = "1qlg5h6j6fsal3lyql8bivbzqj56j5d1vvqi7as8fqvmvr4g22zf";
        };
      }));
      reducers       = doJailbreak old.reducers;
      resourcet      = doJailbreak old.resourcet;
      sdl2           = doJailbreak (haskell.lib.overrideCabal old.sdl2 (oldAttrs: {
        buildDepends = [ new.linear new.text new.vector ];
        src          = pkgs.fetchgit {
                url    = https://github.com/haskell-game/sdl2.git;
                rev    = "02a535bc44ddf1a520b5d0eada648b2801f94a32";
                sha256 = "1y0prl6gllm8xsidq702n576vfd6xmib4v2kip0afpx6z6mnhgdm";
        };
      }));
      semigroupoids  = dontCheck (doJailbreak old.semigroupoids);
      th-expand-syns = haskell.lib.overrideCabal old.th-expand-syns (oldAttrs: {
        src          = pkgs.fetchgit {
                url    = https://github.com/DanielSchuessler/th-expand-syns;
                rev    = "55fbd67f70b7518e60ebf18cf559c7d1b8f0d5fe";
                sha256 = "0frzz6f6zmk2p51903cypk8ab8bjwadcc4i60ld743rcc7gm6ixc";
        };
      });
      trifecta       = doJailbreak old.trifecta;
      optparse-generic = ghc.callPackage ({ mkDerivation, base, optparse-applicative, system-filepath, text, transformers, void }:
             mkDerivation {
               pname = "optparse-generic";
               version = "1.0.0";
               buildDepends = [ base optparse-applicative system-filepath text transformers void ];
               src = pkgs.fetchgit {
                 url    = https://github.com/Gabriel439/Haskell-Optparse-Generic-Library.git;
                 rev    = "d0a26abb9eb883bd9fff07773f5ce987a7215023";
                 sha256 = "1y76wmsr7pcfddgy26h4ra72c0l3fp01ck9j868qy9qxgbcnh6m0";
               };
               isLibrary = true;
               isExecutable = false;
               license = stdenv.lib.licenses.gpl3;
             }) {};
    };
  });
  pkgf = import ./.;
  drv  = ghc.callPackage pkgf {
    # hoogle = callPackage ./hoogle.nix { }
  };
in with pkgs;
  (haskell.lib.addBuildTools drv [
    # ghc.cabal-install
    # ghc.halive
    # ghc.hoogle-index
    ##
    # emacs git ltrace silver-searcher strace
  ]).env
