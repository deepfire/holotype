all: doc

doc:
	cabal haddock --hyperlink-source --hoogle --executables --hscolour=dist/doc/html/mood/mood/src/hscolour.css

