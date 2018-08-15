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
  lambdacube-compiler = overrideCabal super.lambdacube-compiler (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "lambdacube3d";
      repo   = "lambdacube-compiler";
      rev    = "b0b09ee4e7578b9ee0fef1aa9ae541018eafb1d8";
      sha256 = "1x24yis7db3c8n5pbpmmm3a5hp1wm7q8bb548s7747hvswblp0df";
    };
    jailbreak       = true;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-gl = overrideCabal super.lambdacube-gl (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "lambdacube3d";
      repo   = "lambdacube-gl";
      rev    = "297828bdcf105c5942ed0e43d9f28130f543f34c";
      sha256 = "1gclb1wn5rl23vsrl1zs3lhiyyddrga6kggrnkpsyi8bwgq8l5z7";
    };
    jailbreak       = true;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-ir = overrideCabal super.lambdacube-ir (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "lambdacube3d";
      repo   = "lambdacube-ir";
      rev    = "b8b4ec9fa985b2eb756ac1a5a65af483a0bd586b";
      sha256 = "0xqm9idapiyl0k93kfi8mh181bxvmda0nf6ipbamlgzxyjcd0314";
    };
    prePatch        = "cd lambdacube-ir.haskell; ";
    jailbreak       = true;
  });

  ## Upstreamed, awaiting a Hackage release
  reflex = overrideCabal super.reflex (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "reflex-frp";
      repo   = "reflex";
      rev    = "39d9392b191ec85052eb8839f6c9f207f9e21885";
      sha256 = "07rzrapnalzvl24nc9vgbzah8qpwqkd2v90ijzchwhlgk9y82lfb";
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


  ## Non-code, configuration-only change

}
