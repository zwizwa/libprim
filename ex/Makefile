
SRC=ex.c gc.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: %.c ex_prims.ss ../prim-tools.ss
	pwd
	mzscheme ex_prims.ss $< >$@

GEN = ex.h_ex_prims

all: $(GEN) ex.a

ex.a: $(OBJ)
	ar rcs ex.a $(OBJ)


include ../Makefile.rules
