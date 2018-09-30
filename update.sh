#!/bin/sh

who=${1:-deepfire}

cabal2nix . > default.nix

if test -n "$1"
then shift
fi
for x in "$@"
do
	if test ${who} = "deepfire"
	then rbase=git@github.com:
	else rbase=https://github.com/
        fi
        nix-prefetch-git $rbase${who}/lambdacube-$x > lambdacube-$x.src.json
done
