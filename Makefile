OBJECTS = main.o types.o debug.o kitten.o

all : clean kitten

clean :
	rm -f *.o kitten

kitten : $(OBJECTS)
	gcc $(OBJECTS) -o $@ -lm

%.o : %.c
	gcc -c $^ -Wall -Werror -DDEBUG -o $@
