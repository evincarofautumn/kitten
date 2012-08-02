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

AR := ar qc
HC := ghc
HLINT := ~/.cabal/bin/hlint
CABAL ?= cabal

TargetDir := ./build

CompTargetFile := kitten
CompTargetPath := $(TargetDir)/build/kitten/$(CompTargetFile)
CompSrcNames := $(basename $(wildcard ./*.hs))
CompSrcFiles := $(addsuffix .hs, $(CompSrcNames))
CompInterDir := $(TargetDir)/hi
CompInterFiles := $(addsuffix .hi, $(CompSrcNames))
CompInterPaths := $(addprefix $(CompInterDir)/, $(CompInterFiles))
CompObjDir := $(TargetDir)/obj
CompObjFiles := $(addsuffix .o, $(CompSrcNames))
CompObjPaths := $(addprefix $(CompObjDir)/, $(CompObjFiles))

LibTargetName := kitten
LibTargetFile := lib$(LibTargetName).a
LibTargetPath := $(TargetDir)/$(LibTargetFile)
LibSrcNames := types debug kitten
LibSrcPaths := $(wildcard ./library/*.c) $(wildcard ./library/*.h)
LibObjFiles := $(addsuffix .o, $(LibSrcNames))
LibObjPaths := $(addprefix $(TargetDir)/, $(LibObjFiles))

TestTargetDir := $(TargetDir)/test
TestInterDir := $(TestTargetDir).inter
TestWarnDir := $(TestTargetDir).warn
TestOutDir := $(TestTargetDir).out
TestErrDir := $(TestTargetDir).err
TestSrcDir := test
TestSrcPaths := $(wildcard $(TestSrcDir)/*.ktn)
TestSrcNames := $(basename $(notdir $(TestSrcPaths)))
TestTargetPaths := $(addprefix $(TestTargetDir)/, $(TestSrcNames))

ifdef DEBUG
  LibDebugFlags := -DDEBUG -g
  TestDebugFlags := -DDEBUG -g
else
  LibDebugFlags :=
  TestDebugFlags :=
endif

LibFlags := -std=c99 -Wall -Werror -Ilibrary

CompFlags := -odir $(CompObjDir) -hidir $(CompInterDir)

define ensure_buildable
@ mkdir -p $(dir $1)
endef

#
# Rules. The vast majority of these are phony.
#

MAKEFLAGS += --warn-undefined-variables --silent
.SECONDARY :

.PHONY : all
all : library compiler tests

.PHONY : configure
configure :
	$(CABAL) configure --builddir=$(TargetDir)

.PHONY : help
help :
	@ cat HELP

.PHONY : clean
clean : clean-library clean-compiler clean-tests

.PHONY : clean-depend
clean-depend :
	@ echo 'Cleaning dependency information ...'
	@ rm -f .depend

.PHONY : clean-library
clean-library :
	@ echo 'Cleaning library build files ...'
	@ rm -f $(LibObjPaths) $(LibTargetPath) .depend

.PHONY : clean-compiler
clean-compiler :
	@ echo 'Cleaning compiler build files ...'
	@ $(CABAL) clean --builddir=$(TargetDir)

.PHONY : clean-tests
clean-tests :
	@ echo 'Cleaning test build files ...'
	@ rm -rf $(TestTargetDir)/* $(TestInterDir)/* $(TestOutDir)/* \
		$(TestErrDir)/* $(TestWarnDir)/*

.PHONY : library
library : .depend $(LibTargetPath)

.PHONY : compiler
compiler :
	$(CABAL) build --builddir=$(TargetDir)

$(CompTargetPath) : compiler

.PHONY : tests
tests : $(CompTargetPath) $(TestTargetPaths)
	@ mkdir -p $(TestOutDir)
	@ mkdir -p $(TestWarnDir)
	@ mkdir -p $(TestErrDir)
	@ echo 'Running tests ...'
	@ ./run-tests.sh

.PHONY : lint
lint : $(CompSrcFiles)
	@ echo 'Linting ( $(CompSrcNames) ) ...'
	-@ $(HLINT) .

# Begin non-phony rules.

$(TestInterDir)/%.c : $(TestSrcDir)/%.ktn $(CompTargetPath)
	$(call ensure_buildable,$@)
	@ mkdir -p $(TestWarnDir)
	@ $(CompTargetPath) $< > $@ 2> $(TestWarnDir)/$(basename $(notdir $@))

$(TestTargetDir)/% : $(TestInterDir)/%.c $(LibTargetPath)
	$(call ensure_buildable,$@)
	@ echo 'Building test $(notdir $@) ...'
	@ $(CC) -std=c99 $< \
		-L$(TargetDir) -l$(LibTargetName) -lm -Ilibrary -o $@ \
		$(TestDebugFlags)

.depend : $(LibSrcPaths)
	$(call ensure_buildable,$@)
	@ $(CC) -MM -o $@ $^ $(LibFlags)

include .depend

$(LibTargetPath) : $(LibObjPaths) .depend
	$(call ensure_buildable,$@)
	@ echo 'Linking runtime library ...'
	@ $(AR) $@ $^

$(TargetDir)/%.o : ./library/%.c .depend
	$(call ensure_buildable,$@)
	@ echo 'Building $< ...'
	@ $(CC) -c $< $(LibFlags) $(LibDebugFlags) -o $@
