
SRC=port.c bytes.c symbol.c scanner.c parser.c tuple.c leaf.c grid.c \
	inexact.c channel.c console.c

MODULE=leaf

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: libprim_leaf.a

libprim_leaf.a: $(OBJ)
	ar rcs libprim_leaf.a $(OBJ)

include ../Makefile.defs
include ../Makefile.rules

# parser: all
#	gcc -g -D _PARSER_TEST_ -I /home/tom/libprim parser.c bytes.o \
#		scanner.o port.o tree.o leaf.o symbol.o -o parser
