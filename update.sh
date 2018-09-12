#!/bin/sh

cabal2nix . > default.nix
for x in ir gl compiler
do nix-prefetch-git git@github.com:deepfire/lambdacube-$x > lambdacube-$x.src.json
done
