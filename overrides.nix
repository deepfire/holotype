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
    # src = pkgs.fetchFromGitHub {
    #   owner  = "deepfire";
    #   repo   = "lambdacube-compiler";
    #   rev    = "e022a56bfeafe8306c7615fb1441364ccd876e8e";
    #   sha256 = "0lfyr55jpk113bgkpv8k4140swdmnqh6cxsds9njm462gna3hffr";
    # };
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-compiler.src.json)) ["date"]);
    # src = ../lambdacube-compiler;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ aeson semigroups exceptions megaparsec ansi-wl-pprint pretty-show lambdacube-ir vector ]);
    jailbreak       = true;
    enableLibraryProfiling = false;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-gl = overrideCabal super.lambdacube-gl (drv: {
    # src = pkgs.fetchFromGitHub {
    #   owner  = "deepfire";
    #   repo   = "lambdacube-gl";
    #   rev    = "afa3956593e42ae0495615680f874292647612a3";
    #   sha256 = "1s7y302qcdlwm9j0kzzk0k21drkyh9zijm6v4igr8kfixz2wmwll";
    # };
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-gl.src.json)) ["date"]);
    # src = ../lambdacube-gl;
    jailbreak       = true;
  });

  ## Upstreamed, awaiting a Hackage release
  lambdacube-ir = overrideCabal super.lambdacube-ir (drv: {
    # src = pkgs.fetchFromGitHub {
    #   owner  = "deepfire";
    #   repo   = "lambdacube-ir";
    #   rev    = "36e2a9a208356657c4589869ab6ad8d60a1b2225";
    #   sha256 = "0b3f11133lhy758chf48k4p9q1sp1zalriwzzqa3kfq6hzxd33w9";
    # };
    src = pkgs.fetchgit (removeAttrs (builtins.fromJSON (builtins.readFile ./lambdacube-ir.src.json)) ["date"]);
    prePatch        = "cd lambdacube-ir.haskell; ";
    jailbreak       = true;
    enableLibraryProfiling = false;
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

  type-map = overrideCabal super.type-map (drv: {
    jailbreak       = true;
  });

}
