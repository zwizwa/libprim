%.o: %.c
	gcc -Wall -g -c $<

SRC=test.c gc.c symbol.c
# ceks.c
OBJ=$(SRC:.c=.o)

all: $(OBJ)
	gcc -o gc-test $(OBJ)


clean:
	rm -rf gc-test *.o *~
