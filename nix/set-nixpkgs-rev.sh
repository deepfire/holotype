#!/bin/sh

rev=$1
upstream=${2:-NixOS}

nix-prefetch-git --no-deepClone https://github.com/${upstream}/nixpkgs ${rev} > $(dirname $0)/pins/default-nixpkgs-src.json
