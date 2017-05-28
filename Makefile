all: doc

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/holotype/holotype/src/hscolour.css

hss hsstress:
	ghc -threaded -eventlog -rtsopts -isrc --make HSstress.hs && $(GDB) ./HSstress $(SCENARIO) +RTS -T $(RTS)

lcs lcstress:
	ghc -threaded -eventlog -rtsopts -isrc --make LCstress.hs && $(GDB) ./LCstress $(SCENARIO) +RTS -T $(RTS)

hols holostress:
	ghc -threaded -eventlog -rtsopts -isrc --make Holostress.hs && ./Holostress +RTS -T -ls -N2

