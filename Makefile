COMPILER = kitten
COMPILER_SOURCES = Main.hs Kitten.hs Value.hs CompileError.hs
COMPILER_INTERMEDIATE = $(COMPILER_SOURCES:.hs=.hi)
COMPILER_OBJECTS = $(COMPILER_SOURCES:.hs=.o)
LIBRARY = libkitten.a
LIBRARY_OBJECTS = types.o debug.o kitten.o

ifdef DEBUG
  DEBUG_LIBRARY = -DDEBUG -g
else
  DEBUG_LIBRARY =
endif

all : library compiler

clean : clean_library clean_compiler

clean_library :
	rm -f $(LIBRARY_OBJECTS) $(LIBRARY)

clean_compiler :
	rm -f $(COMPILER_OBJECTS) $(COMPILER_INTERMEDIATE) $(COMPILER)

library : clean_library $(LIBRARY)

compiler : clean_compiler $(COMPILER)

$(LIBRARY) : $(LIBRARY_OBJECTS)
	ar qc $@ $^

%.o : %.c
	gcc -c $^ -Wall -Werror $(DEBUG_LIBRARY) -o $@

debug.c : debug.h
kitten.c : debug.h kitten.h
types.c : debug.h kitten.h types.h
kitten.h : debug.h

$(COMPILER) : $(COMPILER_SOURCES)
	ghc --make Main -package parsec -Wall -o $@
