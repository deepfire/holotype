#!/bin/sh

who=${1:-deepfire}

cabal2nix . > $(dirname $0)/default.nix

if test -n "$1"
then shift
fi
for x in "$@"
do case ${who} in
     # deepfire )     rbase=git@github.com:     ;;
     local | home ) rbase=file://$HOME; who='';;
     * )            rbase=https://github.com/ ;;
   esac
   nix-prefetch-git $rbase${who}/$x > $(dirname $0)/pins/$x.src.json
done
