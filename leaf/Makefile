
SRC=port.c bytes.c symbol.c scanner.c parser.c tree.c



OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: libprim_leaf.a

libprim_leaf.a: $(OBJ)
	ar rcs libprim_leaf.a $(OBJ)

include ../Makefile.defs
include ../Makefile.rules
