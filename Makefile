HASKELL = Main.hs Kitten.hs Value.hs
OBJECTS = types.o debug.o kitten.o
LIBRARY = libkitten.a
COMPILER = kitten

all : clean $(LIBRARY) $(COMPILER)

clean :
	rm -f $(OBJECTS) $(LIBRARY)

$(LIBRARY) : $(OBJECTS)
	ar qc $@ $^

%.o : %.c
	gcc -c $^ -Wall -Werror -DDEBUG -o $@

$(COMPILER) : $(HASKELL)
	ghc --make Main -package parsec -Wall -Werror -o $@
