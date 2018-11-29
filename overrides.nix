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
      rev    = "59ef31edc761109dbe1fc9489af6c620e6bea31e";
      sha256 = "16i8f2407gpr6c29bcxv5fgxlbl4qh2hb6a9d8sag355149wslmq";
    };
    doCheck         = false;
    jailbreak       = true;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ (with self; [ lens data-default hlint filemanip monad-control monoidal-containers prim-uniq unbounded-delays MemoTrie ]);
  });

  ## Upstreamed, awaiting a Hackage release
  singletons = overrideCabal super.singletons (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "goldfirere";
      repo   = "singletons";
      rev    = "f535872b292a14a74a710ce05bcc782269a76793";
      sha256 = "14438xmq795rzkdclyk8d43475xylaz41d4b2gbsv26y8ca2a027";
    };
    jailbreak       = true;
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

  ## Upstreamed, awaiting a Hackage release
  th-desugar = overrideCabal super.th-desugar (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "goldfirere";
      repo   = "th-desugar";
      rev    = "1d164b669079fe57181af95bbbe4749dbf08ab78";
      sha256 = "18ka3cwj05mr4k8w5gz1zis97qmnkasg8yx77vipi27m6mhs059i";
    };
    doCheck         = false;
    jailbreak       = true;
    editedCabalFile = null;
    revision        = null;
  });

  ## Upstreamed, awaiting a Hackage release
  TypeCompose = overrideCabal super.TypeCompose (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "conal";
      repo   = "TypeCompose";
      rev    = "5100cd68b68382b6b65c8c0598a3f34dc9481db3";
      sha256 = "0kh3lncszb66agcsnnhfy6pfxv1jpm8ymrynw5dnavrialbrm9v4";
    };
  });

  ## Upstreamed, awaiting a Hackage release
  type-map = overrideCabal super.type-map (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "Lysxia";
      repo   = "type-map";
      rev    = "2e039e9426976a074a66a31c5750093b52ffafc1";
      sha256 = "15jczrvqyz4alywa9mbmlbav2l4j7k9ql2gv2bxpym3809k8plvj";
    };
    jailbreak       = true;
  });


  ## Unmerged

  ## Unmerged.  PR: https://github.com/gtk2hs/gtk2hs/pull/255
  gtk2hs-buildtools = overrideCabal super.gtk2hs-buildtools (drv: {
    src = pkgs.fetchFromGitHub {
      owner  = "vmchale";
      repo   = "gtk2hs";
      rev    = "8ebc79573158fb5265ca40a322ed9f018c42953e";
      sha256 = "0z6bfcdm9i776bxgmzk6cfw6bpnfv47jvn84v574frbrzksm0yms";
    };
    prePatch        = "cd tools; ";
    jailbreak       = true;
  });

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


  ## Non-code, configuration-only change

  fused-effects = overrideCabal super.fused-effects (drv: {
    jailbreak       = true;
  });

  hoogle-index = overrideCabal super.hoogle-index (drv: {
    jailbreak       = true;
  });

  ref-tf = overrideCabal super.ref-tf (drv: {
    jailbreak       = true;
  });

  stylish-haskell = overrideCabal super.stylish-haskell (drv: {
    jailbreak       = true;
  });

}
