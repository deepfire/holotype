all: holotype

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/holotype/holotype/src/hscolour.css

hss hsstress:
	ghc -threaded -eventlog -rtsopts -isrc --make HSstress.hs && $(GDB) ./HSstress $(SCENARIO) +RTS -T $(RTS)

lcs lcstress:
	ghc -threaded -eventlog -rtsopts -isrc --make LCstress.hs && $(GDB) ./LCstress $(SCENARIO) +RTS -T $(RTS)

hols holostress:
	ghc -threaded -eventlog -rtsopts -isrc --make Holostress.hs && ./Holostress +RTS -T -ls -N2

SRCS=$(wildcard src/*.hs)
dist/build/holotype/holotype: $(SRCS)
	cabal build exe:holotype

#
#
clean:
	cabal clean

#
#
leakcheck_extra_opts := $(and $(full),--track-origins=yes --expensive-definedness-checks=yes)

tool      := $(and $(leakcheck),valgrind --leak-check=full --show-leak-kinds=all $(leakcheck_extra_opts)) \
             $(and $(pprof),pprof)
toollog   := $(and $(leakcheck), 2>&1 | ts -s | tee leakcheck.$(shell date +%s).report) \
             $(and $(pprof),     2>&1 | ts -s | tee     pprof.$(shell date +%s).report)
holotype: dist/build/holotype/holotype
	$(tool) $< +RTS -T $(toollog)
