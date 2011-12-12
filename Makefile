COMPILER = kitten
COMPILER_INTERMEDIATE = Main.hi Kitten.hi Value.hi
COMPILER_OBJECTS = Main.o Kitten.o Value.o
COMPILER_SOURCES = Main.hs Kitten.hs Value.hs
LIBRARY = libkitten.a
LIBRARY_OBJECTS = types.o debug.o kitten.o

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
	gcc -c $^ -Wall -Werror -DDEBUG -o $@

$(COMPILER) : $(COMPILER_SOURCES)
	ghc --make $^ -package parsec -Wall -Werror -o $@
