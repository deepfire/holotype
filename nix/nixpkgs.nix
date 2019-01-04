{ compiler    ? import ./default-compiler.nix
, local       ? false
, lib         ? import ./lib.nix
, nixpkgs     ? lib.nixpkgs
}:

let
  overlays = [
    (_: pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override (oldArgs: {
            overrides = self: super:
                        let parent = (oldArgs.overrides or (_: _: {})) self super;
                        in parent // import ./overrides.nix         { inherit self super pkgs lib; };
          });
        };
      };
    })
    (_: pkgs: {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override (oldArgs: {
            overrides = self: super:
                        let parent = (oldArgs.overrides or (_: _: {})) self super;
                        in parent // import ./manual-overrides.nix { inherit self super pkgs lib; }
                        // {
                          holotype = self.callPackage (import ./default.nix) {};
                        };
          });
        };
      };
    })
  ];
in
  import nixpkgs { inherit overlays; }
