#!/bin/sh

who=${1:-deepfire}
shift

cabal2nix . > default.nix
for x in "$@"
do
	if test ${who} = "deepfire"
	then rbase=git@github.com:
	else rbase=https://github.com/
        fi
        nix-prefetch-git $rbase${who}/lambdacube-$x > lambdacube-$x.src.json
done
