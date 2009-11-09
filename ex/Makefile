
SRC=ex.c gc.c read.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: %.c ex_prims.ss ../prim-tools.ss
	$(MZSCHEME) ex_prims.ss $< >$@
%.h_leg: %.leg
	leg <$< >$@

GEN = ex.h_ex_prims sexp.h_leg

all: $(GEN) libprim_ex.a

TEST = test
TEST_OBJ = test.o ex.a ../leaf/leaf.a

# Note: root deps need to be last!
libprim_ex.a: $(OBJ) 
	ar rcs libprim_ex.a $(OBJ)


include ../Makefile.defs
include ../Makefile.rules
