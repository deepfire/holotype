all: doc

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/holotype/holotype/src/hscolour.css

lcs lcstress:
	ghc -threaded -eventlog -rtsopts -isrc --make LCstress.hs && $(GDB) ./LCstress $(MODE) +RTS -T -ls -N2

hs holostress:
	ghc -threaded -eventlog -rtsopts -isrc --make Holostress.hs && ./Holostress +RTS -T -ls -N2

