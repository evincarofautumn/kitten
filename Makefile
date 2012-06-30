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

HC := ghc
HLINT := ~/.cabal/bin/hlint

TargetDir := ./build

CompTargetFile := kitten
CompTargetPath := $(TargetDir)/$(CompTargetFile)
CompSrcNames := Main Kitten Value CompileError
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
LibSrcPaths := $(wildcard ./*.c) $(wildcard ./*.h)
LibObjFiles := $(addsuffix .o, $(LibSrcNames))
LibObjPaths := $(addprefix $(TargetDir)/, $(LibObjFiles))

TestSrcDir := test
TestTargetDir := $(TargetDir)/test
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

LibFlags := -std=c99 -Wall -Werror

CompFlags := -odir $(CompObjDir) -hidir $(CompInterDir)

#
# Rules. The vast majority of these are phony.
#

MAKEFLAGS += --warn-undefined-variables

.PHONY : all paths tests clean clean_library clean_compiler clean_tests \
    library compiler lint tests build_tests run_tests

all : paths library compiler lint tests

clean : clean_library clean_compiler clean_tests

paths :
	@ echo 'Making sure paths are sane ...'
	@ mkdir -p $(TargetDir)
	@ mkdir -p $(TestTargetDir)
	@ mkdir -p $(CompObjDir)
	@ mkdir -p $(CompInterDir)

clean_depend :
	@ echo 'Cleaning dependency information ...'
	@ rm -f .depend

clean_library :
	@ echo 'Cleaning library build files ...'
	@ rm -f $(LibObjPaths) $(LibTargetPath)

clean_compiler :
	@ echo 'Cleaning compiler build files ...'
	@ rm -f $(CompObjPaths) $(CompInterPaths) $(CompTargetPath)

clean_tests :
	@ echo 'Cleaning test build files ...'
	@ rm -f $(TestTargetDir)/*

library : .depend $(LibTargetPath)
compiler : $(CompTargetPath)
tests : build_tests run_tests

lint : $(CompSrcFiles)
	@ echo 'Linting ( $(CompSrcNames) ) ...'
	-@ $(HLINT) .

run_tests : build_tests $(TestTargetPaths)
	@ echo 'TODO: Run tests.'

$(TestTargetDir)/%.c : $(TestSrcDir)/%.ktn $(CompTargetPath)
	@ echo 'Building test $< ...'
	-@ $(CompTargetPath) $< > $@ 2> $(@:.c=.warn)

$(TestTargetDir)/% : $(TestTargetDir)/%.c
	@ echo 'Building test $@ ...'
	-@ $(CC) -std=c99 $< -L$(TargetDir) -l$(LibTargetName) -lm -I. -o $@ \
		2> $(<:.c=.fail) $(TestDebugFlags)

.depend : $(LibSrcPaths)
	@ $(CC) -MM -o $@ $^ $(LibFlags)

include .depend

$(LibTargetPath) : $(LibObjPaths) .depend
	@ echo 'Linking runtime library ...'
	@ ar qc $@ $^

$(TargetDir)/%.o : ./%.c .depend
	@ echo 'Building $< ...'
	@ $(CC) -c $< $(LibFlags) $(LibDebugFlags) -o $@

# TODO: Move to cabal.
$(CompTargetPath) : $(CompObjPath)
	@ echo 'Building compiler ...'
	@ $(HC) --make Main -package parsec -Wall -o $@ $(CompFlags)
