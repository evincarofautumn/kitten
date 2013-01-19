AR := ar qc
HC := ghc
HLINT := hlint
CABAL ?= cabal

TargetDir := ./build

MAKEFLAGS += --warn-undefined-variables
.SECONDARY :

.PHONY : all
all : compiler lint

.PHONY : clean
clean :
	$(CABAL) clean --builddir=$(TargetDir)

.PHONY : configure
configure :
	$(CABAL) configure --builddir=$(TargetDir)

.PHONY : deps
deps :
	$(CABAL) install --only-dependencies --builddir=$(TargetDir)

.PHONY : compiler
compiler :
	$(CABAL) build --builddir=$(TargetDir)

.PHONY : lint
lint : $(CompSrcFiles)
	@ $(HLINT) .
