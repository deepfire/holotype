#!/bin/sh
#set -e

stopAt=${1:-Main}

topomodules=$(graphmod --toposort --no-cabal Main.hs -isrc 2>/dev/null |
                      awk -e "/^${stopAt}\$/,/^Main\$/ { next }; { print }")

cat <<EOF
---
--- Merged modules:  $(echo ${topomodules})
---
EOF

topoexcludes=$(echo '^\(import@*&\|[&]+\)\('$topomodules'\)\($\|&@*$\|(@*$\)' |
                      sed 's, ,\\|,g; s,\.,\\.,g; s,@,\.,g; s,&, ,g')
alllangpragmas=$(awk -e '/^[ ]*AllowAmbiguousTypes/,/^$$/ { print; }' holotype.cabal | tr -d , | xargs echo)

for p in ${alllangpragmas} TemplateHaskell
do echo "{-# LANGUAGE $p #-}"
done

for module in $topomodules
do
        file=$(echo $module | sed 's,^,src/,; s,\.,/,g; s,$,\.hs,')
        sed -e ':a' -e '/^import {-# SOURCE #-}$/N; s/\n//' $file |
                grep --no-filename '^import ' |
                grep -v '{-# \(SOURCE\)' |
                grep -v "${topoexcludes}"
done | sort | uniq

for module in $topomodules
do
        file=$(echo $module | sed 's,^,src/,; s,\.,/,g; s,$,\.hs,')
        cat <<EOF
---
--- Module $module (from ${file})
---
EOF
        sed -e '/^import {-# SOURCE #-}$/N; s/\n//' $file |
                grep -v --no-filename '^import \|OPTIONS_GHC\|LANGUAGE' |
                awk -e '
/^module /,/^[ )]*where$/ { next }
                     { print }
'
done

cat <<EOF

---
---
---
main = undefined
EOF
