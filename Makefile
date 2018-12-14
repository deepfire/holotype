GHC  ?= $(shell echo $$(cat default-compiler.nix) | tr -d '"')
GHCD := $(shell echo $(GHC) | sed 's/ghc843/8.4.3/;s/ghc861/8.6.1/')
$(info Using ghc-$(GHCD))

all: holotype

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/holotype/holotype/src/hscolour.css

default.nix: holotype.cabal
	cabal2nix --no-haddock --no-check . > default.nix

# LTRACE_TRACE_SPEC=-o holotype.ltrace -e '*pango*@MAIN' -e '*cairo*@MAIN' -e '*g_*@MAIN-g_malloc-g_free-g_strndup'
LTRACE_TRACE_SPEC=-o holotype.ltrace -e '-poll-write-__errno_location'
LTRACE_OPTIONS=--no-signals $(LTRACE_TRACE_SPEC)
#
#
Holostress:     Holostress.hs  src/*.hs
	ghc $< -isrc
hls holostress: Holostress
	./$< $(SCENARIO) +RTS -T $(RTS)

Holoframe:      Holoframe.hs   src/*.hs
	ghc $< -isrc
hf holoframe: Holoframe
	./$< $(SCENARIO) +RTS -T $(RTS)

Refstress:      Refstress.hs   src/*.hs
	ghc $<
rfs refstress: Refstress
	./$< $(SCENARIO) +RTS -T $(RTS)

# hols holostress:
# 	ghc -threaded -eventlog -rtsopts -isrc --make Holostress.hs && ./Holostress +RTS -T -ls -N2
LCstress:     LCstress.hs    src/*.hs
	ghc $<
lcs lcstress: LCstress
	./$< $(SCENARIO) +RTS -T $(RTS)

Cairostress:    Cairostress.hs src/*.hs
	ghc $<
crs crstress:   Cairostress
	./$< $(SCENARIO) # +RTS -T $(RTS)
crsl crstressl: Cairostress
	ltrace $(LTRACE_OPTIONS) $< $(SCENARIO) +RTS -T $(RTS)
	cut -d '(' -f1 holotype.ltrace | sort | uniq -c | grep -v 'resumed>' | sort -n | tee crstress.lprof

cairostress: cairostress.c
	gcc $< -g -o $@ $$(pkg-config --cflags cairo pango pangocairo) $$(pkg-config --libs cairo pango pangocairo)
ccrs: cairostress
	./cairostress

SRCS=$(wildcard *.hs src/*.hs src/*/*.hs src/*/*/*.hs)
BUILDBASE=dist/build
HOLOTYPE=$(BUILDBASE)/holotype/holotype
# BUILDBASE=dist-newstyle/build/x86_64-linux/ghc-$(GHCD)/holotype-0.0.1/x
# HOLOTYPE=$(BUILDBASE)/holotype/build/holotype/holotype
$(HOLOTYPE): $(SRCS)
	cabal build -j4 exe:holotype

#
#
clean:
	cabal clean
	rm dist dist-newstyle .ghc.environment.* -rf
	rm -f {,src/}*.{o,hi,dyn_hi,dyn_o,hs~} *~ *.hp *.lprof *.bin
cls:
	echo -en '\ec'

# Support for experiments:
#
define defexperiment =
$(BUILDBASE)/$1/build/$1/$1: ./experiments/$1/Main.hs
	@cabal -v0 new-build $1
$1: $(BUILDBASE)/$1/build/$1/$1
	@$(BUILDBASE)/$1/build/$1/$1
endef
experiments := $(shell find experiments -maxdepth 1 -mindepth 1 -type d | grep -vw dist | cut -d/ -f2)
$(foreach x,$(experiments),$(eval $(call defexperiment,$(x))))
# Current experiment:
#
n          ?= new
exnm       := $(n)
exdir       = ./experiments/$(exnm)
exdepsbase := base, base-unicode-symbols, containers, lens, TypeCompose
deps       ?=
exdeps     := $(exdepsbase)$(if $(deps), $(deps))
ls:
	@echo "experiments:"
	@echo
	@for x in $(experiments); do echo "  $${x}"; done
new new-experiment:
	@test -d $(exdir) && { echo "ERROR: experiment '$(exnm)' already present in '$(exdir)'"; exit 1; } || true
	mkdir -p $(exdir)
	cp experiments/skeleton.hs $(exdir)/Main.hs
	@echo ""                                           >> holotype.cabal
	@echo "executable $(exnm)"                         >> holotype.cabal
	@echo "  hs-source-dirs:      experiments/$(exnm)" >> holotype.cabal
	@echo "  main-is:             Main.hs"             >> holotype.cabal
	@echo "  default-language:    Haskell2010"         >> holotype.cabal
	@echo "  build-depends:       $(exdeps)"           >> holotype.cabal

	@echo "Prepared new experiment '$(exnm)' in '$(exdir)':"
	@echo
	@echo "  running 'make $(exnm)':"
	@echo
	@make --no-print-directory $(exnm)

#
#
leakcheck_extra_opts := $(and $(full),--track-origins=yes --expensive-definedness-checks=yes)

tool      := $(and $(leakcheck),valgrind --leak-check=full --show-leak-kinds=all $(leakcheck_extra_opts)) \
             $(and $(pprof),pprof)
toollog   := $(and $(leakcheck), 2>&1 | ts -s | tee leakcheck.$(shell date +%s).report) \
             $(and $(pprof),     2>&1 | ts -s | tee     pprof.$(shell date +%s).report)
traced: OPTS=--trace
traced: holotype
holotype: $(HOLOTYPE)
	$(tool) $< +RTS -T -RTS $(OPTS) $(toollog)
pholotype: $(HOLOTYPE)
	$(tool) $< +RTS -T -h -l $(toollog)
	hp2ps -c holotype.hp
	evince holotype.ps


RESOURCE_CALLS='pango_font_map_create_context|pango_cairo_create_context|pango_layout_new|cairo_destroy|cairo_surface_destroy|cairo_create|cairo_image_surface_create'
lholotype: $(HOLOTYPE)
	ltrace $(LTRACE_OPTIONS) $< +RTS -T -RTS $(OPTS) $(toollog)
	cut -d '(' -f1 holotype.ltrace | sort | uniq -c | grep -v 'resumed>' | sort -n | tee holotype.lprof
res resources:
	egrep $(RESOURCE_CALLS) holotype.lprof

#
#
pro prorun pronounce-runnable:
	git branch -f runnable HEAD
package:
package:
	nix-build packages.nix -A ${NAME} --argstr compiler ghc${GHC} --show-trace --cores 0 --no-out-link
list-shell-failures:
list-shell-failures:
	nix-shell shell.nix               --argstr compiler ghc${GHC} --show-trace --cores 1 --max-jobs 1 --keep-going

.PHONY: overrides.nix
overrides.nix:
	nh overrides-descs > $@

.PHONY: refs references
refs references:
	nix-store --query --references $$(nix-instantiate package.nix)

.PHONY: gdb
gdb: $(HOLOTYPE)
	gdb -ex run --args $(HOLOTYPE) +RTS -V0 -l-au

.PHONY: modules graph
modules graph:
	graphmod | dot -Tpdf > holotype.pdf && evince holotype.pdf
