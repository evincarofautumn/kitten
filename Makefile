#
# Constants for use in rules. The abbreviations "Comp", "Lib", "Src", "Inter",
# and "Obj" are used to differentiate those constants related to the compiler
# and runtime library, as well as source files, intermediate files, and object
# files, respectively. Constants ending with "Dir" refer to directories only
# (without trailing slashes); "Flags", to commandline options for individual
# build tools; "Name(s)", to bare filenames with no extensions or directories,
# and "Path(s)", to full paths including filenames, extensions, and directories.
#
# TODO: Add constants for build tools that may differ between platforms.
#

.PHONY : all
all : tests

AR := ar qc
HC := ghc
HLINT := ~/.cabal/bin/hlint
CABAL ?= cabal

TargetDir := ./build

LibFlags := -std=c99 -Wall -Werror -Ilibrary
CompFlags := -odir $(CompObjDir) -hidir $(CompInterDir)

define ensure_buildable
@ mkdir -p $(dir $1)
endef

include Makefile.library
include Makefile.compiler

include Makefile.tests

#
# Rules. The vast majority of these are phony.
#

MAKEFLAGS += --warn-undefined-variables --silent
.SECONDARY :

.PHONY : configure
configure :
	$(CABAL) configure --builddir=$(TargetDir)

.PHONY : help
help :
	@ cat HELP

.PHONY : clean
clean : clean-library clean-compiler clean-tests

.PHONY : lint
lint : $(CompSrcFiles)
	@ echo 'Linting ( $(CompSrcNames) ) ...'
	-@ $(HLINT) .
