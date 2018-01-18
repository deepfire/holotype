{ pkgs, haskellLib, super, self }:

with haskellLib; with self; {
# Released

  blaze-markup = overrideCabal super.blaze-markup (drv: {
    version         = "0.8.2.0";
    sha256          = "0m3h3ryxj5r74mv5g5dnfq5jbbwmvkl7ray18vi20d5vd93sydj4";
    doCheck         = false;
    jailbreak       = true;
  });

  conduit = overrideCabal super.conduit (drv: {
    version         = "1.2.13";
    sha256          = "1b0i6zbmp9j0km150nghmq77rz3iahkib3dd2m9hihabc6n1p793";
    doCheck         = false;
    jailbreak       = true;
  });

  generic-deriving = overrideCabal super.generic-deriving (drv: {
    version         = "1.12.1";
    sha256          = "0wwl29f5mlxmrigh0kp35q7aj10ymknnjabmdrdfxpi079rkzzgm";
    doCheck         = false;
    jailbreak       = true;
  });

  haskell-src-exts-util = overrideCabal super.haskell-src-exts-util (drv: {
    version         = "0.2.2";
    sha256          = "14rhwcrdz3kfb69c64qn8kybl7wnpajrjlfz5p95ca4bva4mwclg";

  });

  hspec = overrideCabal super.hspec (drv: {
    version         = "2.4.7";
    sha256          = "1jvf7x43gkch4b8nxqdascqlh4rh2d1qvl44skwqkz0gw154ldan";
    doHaddock       = false;
  });

  hspec-core = overrideCabal super.hspec-core (drv: {
    version         = "2.4.7";
    sha256          = "0syjbx3s62shwddp75qj0nfwmfjn0yflja4bh23x161xpx1g0igx";
    doHaddock       = false;
  });

  hspec-discover = overrideCabal super.hspec-discover (drv: {
    version         = "2.4.7";
    sha256          = "1cgj6c6f5vpn36jg2j7v80nr87x1dsf7qyvxvjw8qimjdxrcx0ba";
    doHaddock       = false;
  });

  hspec-meta = overrideCabal super.hspec-meta (drv: {
    version         = "2.4.6";
    sha256          = "0qmvk01n79j6skn79r6zalg2pd0x0nqqn9qn8mhg0pgyzcdnfc9b";
    doHaddock       = false;
  });

  microlens = overrideCabal super.microlens (drv: {
    version         = "0.4.8.3";
    sha256          = "17qx2mbqdrlnkc3gxq8njbp7qw8nh51drmz6fc8khgj9bls5ni2k";

  });

  microlens-mtl = overrideCabal super.microlens-mtl (drv: {
    version         = "0.1.11.1";
    sha256          = "0l6z1gkzwcpv89bxf5vgfrjb6gq2pj7sjjc53nvi5b9alx34zryk";
    doHaddock       = false;
  });

  microlens-th = overrideCabal super.microlens-th (drv: {
    version         = "0.4.1.3";
    sha256          = "15a12cqxlgbcn1n73zwrxnp2vfm8b0ma0a0sdd8zmjbs8zy3np4f";
    doHaddock       = false;
  });

  nanospec = overrideCabal super.nanospec (drv: {
    version         = "0.2.2";
    sha256          = "1rcmhl9bhyfvanalnf1r86wkx6rq6wdvagnw1h011jcnnb1cq56g";
    jailbreak       = true;
  });

  pretty-show = overrideCabal super.pretty-show (drv: {
    version         = "1.6.16";
    sha256          = "0l03mhbdnf0sj6kw2s3cf2xhfbl0809jr9fhj7cmpkhjpxv89vnv";

  });

  singletons = overrideCabal super.singletons (drv: {
    version         = "2.4.1";
    sha256          = "1kzrl9njvkbvxylk9jg61vy3ksmxmzymci5hdp0ilpsah4620yjx";
    doCheck         = false;
    jailbreak       = true;
  });

  tasty = overrideCabal super.tasty (drv: {
    version         = "1.0.0.1";
    sha256          = "0ggqffw9kbb6nlq1pplk131qzxndqqzqyf4s2p7576nljx11a7qf";

  });

  test-framework = overrideCabal super.test-framework (drv: {
    version         = "0.8.2.0";
    sha256          = "1hhacrzam6b8f10hyldmjw8pb7frdxh04rfg3farxcxwbnhwgbpm";
    doCheck         = false;
    jailbreak       = true;
    editedCabalFile = null;
    revision        = null;
  });

  xml-conduit = overrideCabal super.xml-conduit (drv: {
    version         = "1.7.1.0";
    sha256          = "1c4ip76qgqjdyf77h97mf3yxdimv7m5ma5v20wchn9qjmbkr8ffa";

  });

  yaml = overrideCabal super.yaml (drv: {
    version         = "0.8.28";
    sha256          = "0swgkzkfrwj0ac7lssn8rnrdfmh3lcsdn5fbq2iwv55di6jbc0pp";

  });


# Upstreamed

  constraints = overrideCabal super.constraints (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/constraints";
      rev    = "07e8ace4d4a842cf450b346ccc75829bd310f4a5";
      sha256 = "11l1hvnvnmzd3zxwddz5di4h5iql47gbwjz73x2kdy21avr85i4v";
    };
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.semigroups self.hspec ];
  });

  doctest = overrideCabal super.doctest (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/sol/doctest";
      rev    = "438ec5a5e551e3f02581fd13064aef9f7d5e0bdc";
      sha256 = "01jkmn37875zn2wlrnipdqk0vqv3amxgaa07w8q3ks2av39sv3wy";
    };
    jailbreak       = true;
  });

  free = overrideCabal super.free (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/free";
      rev    = "fcefc71ed302f2eaf60f020046bad392338b3109";
      sha256 = "0mfrd7y97pgqmb2i66jn5xwjpcrgnfcqq8dzkxqgx1b5wjdydq70";
    };
    jailbreak       = true;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.transformers-base ];
  });

  gtk2hs-buildtools = overrideCabal super.gtk2hs-buildtools (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/gtk2hs/gtk2hs";
      rev    = "1a4065e61e6bef674791bd0463efe3bc27d31816";
      sha256 = "15zj6zgshmpf6dzja3npxjbg56dr0hdyg1qvx5ssg7i56dc1kdxf";
    };
    jailbreak       = true;
    prePatch        = "cd tools; ";
  });

  happy = overrideCabal super.happy (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/simonmar/happy";
      rev    = "8e4dc4318a8e03bbb746ffa0ded1933b1da9e361";
      sha256 = "1vvsc955ms2wfy8n4yjwcgywx679yb5c11iaw1yqy7qb9zfx8zhb";
    };
    doHaddock       = false;
    postPatch       = "rm -f src/AttrGrammarParser.ly src/Parser.ly tests/ParGF.yg";
  });

  haskell-gi = overrideCabal super.haskell-gi (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/haskell-gi/haskell-gi";
      rev    = "30d2e6415c5b57760f8754cd3003eb07483d60e6";
      sha256 = "1l3qm97gcjih695hhj80rbpnd72prnc81lg5y373yj8jk9f6ypbr";
    };
    doCheck         = false;
  });

  haskell-gi-base = overrideCabal super.haskell-gi-base (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/haskell-gi/haskell-gi";
      rev    = "30d2e6415c5b57760f8754cd3003eb07483d60e6";
      sha256 = "1l3qm97gcjih695hhj80rbpnd72prnc81lg5y373yj8jk9f6ypbr";
    };
    doCheck         = false;
    jailbreak       = true;
    prePatch        = "cd base; ";
  });

  haskell-src-meta = overrideCabal super.haskell-src-meta (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/bmillwood/haskell-src-meta";
      rev    = "876a4987fbad019f1c8fdd9ca0bdcce0cc7572a2";
      sha256 = "0qc9fmjpkhkmmm9qxiv6zd4w8p8r4015z9sf1fim1habma4bwajw";
    };
    jailbreak       = true;
  });

  JuicyPixels = overrideCabal super.JuicyPixels (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/Twinside/Juicy.Pixels";
      rev    = "2797ace53b8c0997580ef32a515e80fe1c615921";
      sha256 = "0cay497r0j1rjiic0nfnxwzazi2pv60isnf646hfpa25n2r8y52p";
    };
    doCheck         = false;
    jailbreak       = true;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.MonadRandom ];
  });

  lambdacube-compiler = overrideCabal super.lambdacube-compiler (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/lambdacube3d/lambdacube-compiler";
      rev    = "ff6e3b136eede172f20ea8a0f7017ad1ecd029b8";
      sha256 = "0srzrq5s7pdbygn7vdipxl12a3gbyb6bpa7frbh8zwhb9fz0jx5m";
    };
    jailbreak       = true;
  });

  lambdacube-gl = overrideCabal super.lambdacube-gl (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/lambdacube3d/lambdacube-gl";
      rev    = "297dfbf6f633a1269640b92ebc55b05ca58c102f";
      sha256 = "13hd8cn3dpqz5mxrvf8a5rqq200icfq5lim73nzhzp8wqw6gw30h";
    };
    jailbreak       = true;
  });

  lambdacube-ir = overrideCabal super.lambdacube-ir (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/lambdacube3d/lambdacube-ir";
      rev    = "b86318b510ef59606c5b7c882cad33af52ce257c";
      sha256 = "0j4r6b32lcm6jg653xzg9ijxkfjahlb4x026mv5dhs18kvgqhr8x";
    };
    jailbreak       = true;
    prePatch        = "cd lambdacube-ir.haskell; ";
  });

  lens = overrideCabal super.lens (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/lens";
      rev    = "4ad49eaf2448d856f0433fe5a4232f1e8fa87eb0";
      sha256 = "0sd08v6syadplhk5d21yi7qffbjncn8z1bqlwz9nyyb0xja8s8wa";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  network-uri = overrideCabal super.network-uri (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/haskell/network-uri";
      rev    = "3b388b2bf07638ae0c2077e4c752d695da303d87";
      sha256 = "0yhkbnjmy3kasc69ydyngr4af9rg3l4g6bm4vrj4h74q08cbiapb";
    };
    jailbreak       = true;
  });

  primitive = overrideCabal super.primitive (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/haskell/primitive";
      rev    = "1090cbd159f23e4d5867348b61badae32bc9ec6c";
      sha256 = "1bssnpyfz4nh6w8gxgxi0da2cdlarr15daa9fidci67f3d81r24k";
    };
    jailbreak       = true;
  });

  profunctors = overrideCabal super.profunctors (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/profunctors";
      rev    = "f9766bcb4a97484d3a00361cd824b9d1a38386d1";
      sha256 = "05pk7aqp81s5z7gck0nf2fvbjvaq4bjfghikm8f0vms09l59c76l";
    };

  });

  reflection = overrideCabal super.reflection (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/reflection";
      rev    = "6508a04342256cab34bd6aee06ec61a166ce56fb";
      sha256 = "05dkwx1p43rc45g9y6bwdxiqfcdhq0anr4djfaraj3a0nain5caf";
    };
    jailbreak       = true;
  });

  semigroupoids = overrideCabal super.semigroupoids (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/semigroupoids";
      rev    = "2ba07fd6e211a4cce981da3ea279fa1d67f69db1";
      sha256 = "1a9dr1099lbny7fd64xp10f5fgbcrma8cw71q07388xhs3g31w42";
    };
    doCheck         = false;
    editedCabalFile = null;
    revision        = null;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.template-haskell ];
  });

  simple-reflect = overrideCabal super.simple-reflect (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/twanvl/simple-reflect";
      rev    = "c357e55da9a712dc5dbbfe6e36394e4ada2db310";
      sha256 = "15q41b00l8y51xzhbj5zhddyh3gi7gvml033w8mm2fih458jf6yq";
    };
    jailbreak       = true;
  });

  stringbuilder = overrideCabal super.stringbuilder (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/sol/stringbuilder";
      rev    = "4a1b689d3c8a462b28e0d21224b96165f622e6f7";
      sha256 = "0h3nva4mwxkdg7hh7b7a3v561wi1bvmj0pshhd3sl7dy3lpvnrah";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  tagged = overrideCabal super.tagged (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ekmett/tagged";
      rev    = "d86a2109773bd79adecf78ed49f85316ff0ae2ad";
      sha256 = "1hfq660rc05ljqhnr65n01kmxvyfad7widwg5qn83a74v41qbhap";
    };
    doCheck         = false;
    jailbreak       = true;
    editedCabalFile = null;
    revision        = null;
  });

  th-desugar = overrideCabal super.th-desugar (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/goldfirere/th-desugar";
      rev    = "879edfa2a09fb5a829b40ccb55de960398bf8cea";
      sha256 = "1p3aac87sfrrs9p4llb63qb891slx2mjz7vf255lvvgcfkhqrcrp";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  unordered-containers = overrideCabal super.unordered-containers (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/tibbe/unordered-containers";
      rev    = "60ced060304840ed0bf368249ed6eb4e43d4cefc";
      sha256 = "10sa6h6cvhrsg8yqg23dg07q28liczgqwa084zyrkpifsg0j3zhq";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  websockets = overrideCabal super.websockets (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/jaspervdj/websockets";
      rev    = "11ba6d15cf47bace1936b13a58192e37908b0300";
      sha256 = "1swphhnqvs5kh0wlqpjjgx9q91yxi6lasid8akdxp3gqll5ii2hf";
    };

  });


# Unmerged

  blaze-builder = overrideCabal super.blaze-builder (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/lpsmith/blaze-builder";
      rev    = "b7195f160795a081adbb9013810d843f1ba5e062";
      sha256 = "1g351fdpsvn2lbqiy9bg2s0wwrdccb8q1zh7gvpsx5nnj24b1c00";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  bytestring-trie = overrideCabal super.bytestring-trie (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/wrengr/bytestring-trie";
      rev    = "e0ae0cb1ad40dedd560090d69cc36f9760797e29";
      sha256 = "1jkdchvrca7dgpij5k4h1dy4qr1rli3fzbsqajwxmx9865rgiksl";
    };
    doCheck         = false;
    jailbreak       = true;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.data-or ];
  });

  cereal = overrideCabal super.cereal (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/GaloisInc/cereal";
      rev    = "c2f233a3dd19655edf40841f91848ea1e78978a6";
      sha256 = "1gfj9vnrk38d3a02wz2w1x9vcdr8k50g0f8djcgcqqkdmh1qrzyh";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  conduit-extras = overrideCabal super.conduit-extras (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com//conduit-extras";
      rev    = "";
      sha256 = "";
    };
    doCheck         = false;
    jailbreak       = true;
    prePatch        = "cd conduit-extras; ";
  });

  hashtables = overrideCabal super.hashtables (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/gregorycollins/hashtables";
      rev    = "b9eb4b10a50bd6250330422afecc065339a32412";
      sha256 = "0l4nplpvnzzf397zyh7j2k6yiqb46k6bdy00m4zzvhlfp7p1xkaw";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  haskell-src-exts = overrideCabal super.haskell-src-exts (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/haskell-suite/haskell-src-exts";
      rev    = "f1ab604faf30672af3581ed1370c8d88d7ebf28f";
      sha256 = "09v26yh1xi7gby1jn9srqq549mpxrkg5qal1ippq5b6ca1ly2grg";
    };
    jailbreak       = true;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.semigroups ];
  });

  hedgehog = overrideCabal super.hedgehog (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/hedgehogqa/haskell-hedgehog";
      rev    = "070264496263d6c6e6708d58870abc9780c1531f";
      sha256 = "02h3vlbl3lvymsq6nf9pybs752lnp5bmf6fi65iy6q3hiypgwryy";
    };
    jailbreak       = true;
    prePatch        = "cd hedgehog; ";
  });

  language-c = overrideCabal super.language-c (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/visq/language-c";
      rev    = "03b120c64c12946d134017f4922b55c6ab4f52f8";
      sha256 = "1mcv46fq37kkd20rhhdbn837han5knjdsgc7ckqp5r2r9m3vy89r";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  language-c_0_7_0 = overrideCabal super.language-c_0_7_0 (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/visq/language-c";
      rev    = "03b120c64c12946d134017f4922b55c6ab4f52f8";
      sha256 = "1mcv46fq37kkd20rhhdbn837han5knjdsgc7ckqp5r2r9m3vy89r";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  MemoTrie = overrideCabal super.MemoTrie (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/conal/MemoTrie";
      rev    = "11f8791c3b29db3351c89cc85faa2dc8068a55ce";
      sha256 = "1r37c7ai5h794a5131yal4n519icim5gh7g9jcd13z02n736hdi2";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  monadplus = overrideCabal super.monadplus (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/hanshoglund/monadplus";
      rev    = "aa09f2473e2c906f2707b8a3fdb0a087405fd6fb";
      sha256 = "0g37s3rih4i3vrn4kjwj12nq5lkpckmjw33xviva9gly2vg6p3xc";
    };
    jailbreak       = true;
  });

  reflex = overrideCabal super.reflex (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/reflex-frp/reflex";
      rev    = "4fb50139db45a37493b91973eeaad9885b4c63ca";
      sha256 = "0i7pp6cw394m2vbwcqv9z5ngdarp01sabqr1jkkgchxdkkii94nx";
    };
    doHaddock       = false;
    jailbreak       = true;
    libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.data-default self.haskell-src-exts self.lens self.monad-control self.prim-uniq self.reflection self.split self.template-haskell self.unbounded-delays ];
  });

  regex-tdfa = overrideCabal super.regex-tdfa (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ChrisKuklewicz/regex-tdfa";
      rev    = "34f4593a520176a917b74b8c7fcbbfbd72fb8178";
      sha256 = "1aiklvf08w1hx2jn9n3sm61mfvdx4fkabszkjliapih2yjpmi3hq";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  tasty-ant-xml = overrideCabal super.tasty-ant-xml (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ocharles/tasty-ant-xml";
      rev    = "4fd48e099ea635e060f9141ddd4d72ada0e6a692";
      sha256 = "14w9c12pf0n90405vmhi9fwggbaq7pwlx7bi71r5x7w1x6x5l9za";
    };
    jailbreak       = true;
  });

  text-format = overrideCabal super.text-format (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/bos/text-format";
      rev    = "a1cda87c222d422816f956c7272e752ea12dbe19";
      sha256 = "0lyrx4l57v15rvazrmw0nfka9iyxs4wyaasjj9y1525va9s1z4fr";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  th-lift = overrideCabal super.th-lift (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/mboes/th-lift";
      rev    = "8087adb22d3b1ff1dcd4d960aa8778d77c9e3538";
      sha256 = "067lxkka8s0pnn84v4ii86psv3l33jkibh81cw2z6x56a97hbji0";
    };
    doCheck         = false;
    jailbreak       = true;
  });

  wl-pprint-text = overrideCabal super.wl-pprint-text (drv: {
    src = pkgs.fetchgit {
      url    = "https://github.com/ivan-m/wl-pprint-text";
      rev    = "615b83d1e5be52d1448aa1ab2517b431a617027b";
      sha256 = "1p67v9s878br0r152h4n37smqhkg78v8zxhf4qm6d035s4rzj76i";
    };
    doCheck         = false;
    jailbreak       = true;
  });


# Non-code change

  adjunctions = overrideCabal super.adjunctions (drv: {
    jailbreak       = true;
  });

  async = overrideCabal super.async (drv: {
    jailbreak       = true;
  });

  bifunctors = overrideCabal super.bifunctors (drv: {
    jailbreak       = true;
  });

  bindings-GLFW = overrideCabal super.bindings-GLFW (drv: {
    jailbreak       = true;
  });

  bytes = overrideCabal super.bytes (drv: {
    doCheck         = false;
  });

  cabal-doctest = overrideCabal super.cabal-doctest (drv: {
    jailbreak       = true;
  });

  ChasingBottoms = overrideCabal super.ChasingBottoms (drv: {
    jailbreak       = true;
  });

  comonad = overrideCabal super.comonad (drv: {
    doCheck         = false;
  });

  distributive = overrideCabal super.distributive (drv: {
    doCheck         = false;
  });

  exceptions = overrideCabal super.exceptions (drv: {
    jailbreak       = true;
  });

  exception-transformers = overrideCabal super.exception-transformers (drv: {
    jailbreak       = true;
  });

  hashable-time = overrideCabal super.hashable-time (drv: {
    jailbreak       = true;
  });

  hlint = overrideCabal super.hlint (drv: {
    jailbreak       = true;
  });

  integer-logarithms = overrideCabal super.integer-logarithms (drv: {
    jailbreak       = true;
  });

  kan-extensions = overrideCabal super.kan-extensions (drv: {
    jailbreak       = true;
  });

  keys = overrideCabal super.keys (drv: {
    jailbreak       = true;
  });

  lifted-async = overrideCabal super.lifted-async (drv: {
    jailbreak       = true;
  });

  linear = overrideCabal super.linear (drv: {
    doCheck         = false;
  });

  newtype-generics = overrideCabal super.newtype-generics (drv: {
    jailbreak       = true;
  });

  parallel = overrideCabal super.parallel (drv: {
    jailbreak       = true;
  });

  parsers = overrideCabal super.parsers (drv: {
    doCheck         = false;
  });

  quickcheck-instances = overrideCabal super.quickcheck-instances (drv: {
    jailbreak       = true;
  });

  scientific = overrideCabal super.scientific (drv: {
    jailbreak       = true;
  });

  split = overrideCabal super.split (drv: {
    jailbreak       = true;
  });

  tasty-expected-failure = overrideCabal super.tasty-expected-failure (drv: {
    jailbreak       = true;
  });

  tasty-hedgehog = overrideCabal super.tasty-hedgehog (drv: {
    jailbreak       = true;
  });

  tasty-hspec = overrideCabal super.tasty-hspec (drv: {
    jailbreak       = true;
  });

  text-lens = overrideCabal super.text-lens (drv: {
    doCheck         = false;
    jailbreak       = true;
  });

  th-abstraction = overrideCabal super.th-abstraction (drv: {
    jailbreak       = true;
  });

  these = overrideCabal super.these (drv: {
    jailbreak       = true;
  });

  trifecta = overrideCabal super.trifecta (drv: {
    doCheck         = false;
  });

  unliftio-core = overrideCabal super.unliftio-core (drv: {
    jailbreak       = true;
  });

  vector = overrideCabal super.vector (drv: {
    jailbreak       = true;
  });

  vector-algorithms = overrideCabal super.vector-algorithms (drv: {
    doCheck         = false;
  });

  wavefront = overrideCabal super.wavefront (drv: {
    jailbreak       = true;
  });

}
