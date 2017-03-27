all: doc

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/holotype/holotype/src/hscolour.css

