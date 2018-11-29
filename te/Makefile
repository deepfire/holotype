NAME:=thinerr-0.1.0

all:
	cabal build
	ghc-pkg unregister $(NAME)
	ls dist/package.conf.inplace/$(NAME)-* | xargs ghc-pkg register --user --force
