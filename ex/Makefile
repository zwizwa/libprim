
SRC=ex.c gc.c ex_prims.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: %.c prims.ss
	pwd
	mzscheme prims.ss $< >$@

GEN = ex_prims.h_ex_prims

all: $(GEN) ex.a

ex.a: $(OBJ)
	ar rcs ex.a $(OBJ)


include ../Makefile.rules
