AR := ar qc
HC := ghc
HLINT := hlint
CABAL ?= cabal

CABALFLAGS ?=
MAKEFLAGS += --warn-undefined-variables
.SECONDARY :

.PHONY : default
default : build test

.PHONY : all
all : deps configure build test lint

.PHONY : clean
clean :
	$(CABAL) clean

.PHONY : configure
configure :
	$(CABAL) configure $(CABALFLAGS)

.PHONY : deps
deps :
	$(CABAL) install $(CABALFLAGS) --only-dependencies

.PHONY : build
build :
	$(CABAL) build

.PHONY: test
test:
	$(CABAL) test $(CABALFLAGS)

.PHONY : lint
lint :
	@ $(HLINT) compiler

.PHONY : loc
loc :
	@ find src lib test -name '*.hs' -exec wc -l {} + | sort -n
