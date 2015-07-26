{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghcNokinds"
}:
let
  pkgs = nixpkgs.pkgs;
  ghc  = pkgs.haskell.packages.${compiler};
  pkgf = import ./.;
  drv  = ghc.callPackage pkgf {};
  ghci-ng      = nixpkgs.pkgs.haskell.lib.overrideCabal ghc.ghci-ng (oldAttrs: {
    buildDepends = oldAttrs.buildDepends ++ [ ghc.syb ];
    src = pkgs.fetchgit {
      url = https://github.com/chrisdone/ghci-ng.git;
      rev = "738f66f3d1f1a3b7ba574fb9c83da793179a42c3";
      sha256 = "03kz2ysmglgnhcbzw03wrgjsf8pmh87f1ajgvdny3rm2mbg37iig";
    };
  });
in with pkgs;
  (pkgs.haskell.lib.addBuildTools drv [
    ghc.cabal-install
    ghc.halive
    ghc.hoogle-index
    ##
    ghci-ng
    ##
    emacs git ltrace silver-searcher strace
  ]).env
