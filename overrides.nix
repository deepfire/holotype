{ self, super, pkgs, ... }:

with pkgs.haskell.lib; with self; {

  ## Shadowed:


  ## On Hackage:


  ## Upstreamed

  ## Upstreamed, awaiting a Hackage release
  graphmod = overrideCabal super.graphmod (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "yav";
      repo   = "graphmod";
      rev    = "b1ba362eb1405dda9b22ed5cba532013cc30f862";
      sha256 = "179kxfjzn3y5xq654ilm3d63nq762l465crvn9ryp0zn3ffvkq8r";
    };
  });

  ## Upstreamed, awaiting a Hackage release
  lens-sop = overrideCabal super.lens-sop (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "well-typed";
      repo   = "lens-sop";
      rev    = "44d160557c216fd36fe40d7e6c84bc93b26568d8";
      sha256 = "1pgzy67ddmcs1zhvan4q9xhj113gpamy4disgg9x4b6fwl3rb91l";
    };
    jailbreak       = true;
  });

  ## Upstreamed, awaiting a Hackage release
  reflex = overrideCabal super.reflex (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "reflex-frp";
      repo   = "reflex";
      rev    = "7180ebfe283d973c5abc5ac851904f488c579a0f";
      sha256 = "1p3gcjlkrz4vs2gsrnzlwqdpkb7wc3rz6adj65d02jdrghx81idj";
    };
    doCheck         = false;
    jailbreak       = true;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ lens data-default hlint filemanip monad-control monoidal-containers prim-uniq unbounded-delays MemoTrie ]);
  });

  ## Upstreamed, awaiting a Hackage release
  text-lens = overrideCabal super.text-lens (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "ChrisPenner";
      repo   = "rasa";
      rev    = "9eb595bf166f0c23c4596d1161ec9b14e2d71b48";
      sha256 = "0p0biq6kaa7jv83rpdj5g3rhflcrqazlf8lbgn8zvfhr31ijkvwk";
    };
    prePatch        = "cd text-lens; ";
    doCheck         = false;
    jailbreak       = true;
  });


  ## Unmerged

  ## Unmerged.  PR: https://github.com/hanshoglund/monadplus/pull/3
  monadplus = overrideCabal super.monadplus (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "asr";
      repo   = "monadplus";
      rev    = "c4231e1423e5c81f2779acf4462eea8872f6a4e6";
      sha256 = "1r82rqpjkkkwm35wrqwwqkba4nbvilf7arcwqiacvwfz5zkp2gf6";
    };
    jailbreak       = true;
  });

  ## Unmerged.  PR: https://github.com/bgamari/monoidal-containers/pull/0
  monoidal-containers = overrideCabal super.monoidal-containers (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "obsidiansystems";
      repo   = "monoidal-containers";
      rev    = "79c25ac6bb469bfa92f8fd226684617b6753e955";
      sha256 = "0j2mwf5zhz7cmn01x9v51w8vpx16hrl9x9rcx8fggf21slva8lf8";
    };
    jailbreak       = true;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ aeson these ]);
  });

  ## Unmerged.  PR: https://github.com/bos/text-format/pull/23
  text-format = overrideCabal super.text-format (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "phadej";
      repo   = "text-format";
      rev    = "fd9598537f8bb4d718459e3b71b64bf9c1e92526";
      sha256 = "16b5821jfsd0lfkllpahvaq344jmhkhvl284k7k375fy1fw20bwq";
    };
    jailbreak       = true;
  });

  ## Unmerged.  PR: https://github.com/conal/TypeCompose/pull/
  TypeCompose = overrideCabal super.TypeCompose (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "deepfire";
      repo   = "TypeCompose";
      rev    = "60230e6da02b74b2e593a2531acc50f706d17fc0";
      sha256 = "0nwylsx93m0iv7rkr5m5np2cmi8p0zfpprbxwzcag6vkja3n0w5g";
    };
  });


  ## Non-code, configuration-only change

  type-map = overrideCabal super.type-map (drv: {
    jailbreak       = true;
  });

}
