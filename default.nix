let
  pkgs     = (import <nixpkgs> {}).pkgs;
  haskell  = pkgs.haskell;
  ghcOrig  = haskell.packages.ghc801;
  # ghc      = ghcOrig;
  ghc      = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
      bifunctors     = dontCheck (doJailbreak old.bifunctors);
      comonad        = dontCheck (doJailbreak old.comonad);
      doctest        = dontCheck (haskell.lib.overrideCabal old.doctest (oldAttrs: {
        buildDepends = [ new.base-compat ];
        src = pkgs.fetchgit {
                url    = https://github.com/sol/doctest;
                rev    = "d042176d41e8466de664198ef473bc2ae280e3e4";
                sha256 = "15ffykfw4jmxqiziiz31yfcm8v4iq3iz9x882xvlvzi5b7b408yk";
        };
      }));
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
      linear         = dontCheck (haskell.lib.overrideCabal old.linear (oldAttrs: {
        src = pkgs.fetchgit {
                url    = https://github.com/ekmett/linear.git;
                rev    = "7de2733b1d922a2717860df49b6090042b81ea35";
                sha256 = "0c3cw4b9cnypi1rjdyzp5xb0qy088q3mg5in8q5lcvqfmpzfc22b";
        };
      }));
      sdl2           = doJailbreak (haskell.lib.overrideCabal old.sdl2 (oldAttrs: {
        buildDepends = [ ghc.linear ghc.text ghc.vector ];
        src          = pkgs.fetchgit {
                url    = https://github.com/haskell-game/sdl2.git;
                rev    = "02a535bc44ddf1a520b5d0eada648b2801f94a32";
                sha256 = "1y0prl6gllm8xsidq702n576vfd6xmib4v2kip0afpx6z6mnhgdm";
		# url    = https://github.com/deepfire/sdl2;
                # rev    = "bb2c4b6b52b48497f3271fc880dd1a0b11623ef7";
                # sha256 = "0fcirg3g2kd9d001hzz60mililp79l36prh0nqpcw0fzk5zzp9y2";
        };
      }));
      semigroupoids  = dontCheck (doJailbreak old.semigroupoids);
    };
  });
in
# ghc.callPackage
# (
#{ stdenv, mkDerivation, base, base-unicode-symbols, containers, diagrams, hscolour, linear, netwire, pkgconfig, reflection, sdl2, vector }:
{ stdenv, mkDerivation, base, base-unicode-symbols, containers,           hscolour, linear, netwire, pkgconfig, reflection, sdl2, vector }:
mkDerivation {
  pname        = "mood";
  version      = "0.0.1";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
# buildDepends = [ base base-unicode-symbols containers diagrams hscolour linear netwire pkgconfig reflection sdl2 vector ];
  buildDepends = [ base base-unicode-symbols containers          hscolour linear netwire pkgconfig reflection sdl2 vector ];
  description  = "Graph-backed visual mind assistant";
  license      = stdenv.lib.licenses.agpl3;
}
# ) {}
