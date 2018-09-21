#!/bin/sh

what="${1:-ir gl compiler}"
who=${2:-deepfire}

cabal2nix . > default.nix
for x in ${what}
do nix-prefetch-git git@github.com:${who}/lambdacube-$x > lambdacube-$x.src.json
done
