
SRC=task.c port.c bytes.c symbol.c 


OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: leaf.a

leaf.a: $(OBJ)
	ar rcs leaf.a $(OBJ)

include ../Makefile.rules
