OBJECTS = types.o debug.o kitten.o
LIBRARY = libkitten.a

all : clean $(LIBRARY)

clean :
	rm -f $(OBJECTS) $(LIBRARY)

$(LIBRARY) : $(OBJECTS)
	ar qc $@ $^

%.o : %.c
	gcc -c $^ -Wall -Werror -DDEBUG -o $@
