
SRC=ex.c gc.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: %.c ex_prims.ss ../prim-tools.ss
	pwd
	mzscheme ex_prims.ss $< >$@
%.h_peg: %.peg
	peg <$< >$@

GEN = ex.h_ex_prims sexp.h_peg

all: $(GEN) ex.a

TEST = test
TEST_OBJ = test.o ex.a ../leaf/leaf.a

# Note: root deps need to be last!
ex.a: $(OBJ) 
	ar rcs ex.a $(OBJ)


include ../Makefile.rules
