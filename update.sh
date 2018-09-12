#!/bin/sh

who=${1:-deepfire}

cabal2nix . > default.nix
for x in ir gl compiler
do nix-prefetch-git git@github.com:${who}/lambdacube-$x > lambdacube-$x.src.json
done
