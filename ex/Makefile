include ../Makefile.defs

MODULE=ex
SRC=ex.c gc.c read_intl.c
# read_leg.c

# %.h_leg: %.leg
# 	leg <$< >$@

GEN = ex.h_prims
# sexp.h_leg

all: $(GEN) libprim_ex.a

TEST = test
TEST_OBJ = test.o ex.a ../leaf/leaf.a

include ../Makefile.rules

tok:
	gcc -I/home/tom/libprim sexp-tok.c ../leaf/bytes.o ../leaf/port.o
