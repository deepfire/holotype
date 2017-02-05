sudo NIX_PATH=nixpkgs=/home/desktop/src/nixpkgs ~desktop/.nix-profile/bin/nix-build ~desktop/src/core/nix/opengl.nix -o /run/opengl-driver
export LD_LIBRARY_PATH=/run/opengl-driver/lib
