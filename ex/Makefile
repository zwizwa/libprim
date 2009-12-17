include ../Makefile.defs

MODULE=ex
SRC=ex.c gc.c read_intl.c

# read_leg.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: $(SRCDIR)/$(MODULE)/%.c $(SRCDIR)/$(MODULE)/ex_prims.ss $(SRCDIR)/prim-tools.ss
	$(MZSCHEME) $(SRCDIR)/$(MODULE)/ex_prims.ss $< >$@
%.h_leg: %.leg
	leg <$< >$@

GEN = ex.h_ex_prims
# sexp.h_leg

all: $(GEN) libprim_ex.a

TEST = test
TEST_OBJ = test.o ex.a ../leaf/leaf.a

# Note: root deps need to be last!
libprim_ex.a: $(OBJ) 
	ar rcs libprim_ex.a $(OBJ)


include ../Makefile.rules


tok:
	gcc -I/home/tom/libprim sexp-tok.c ../leaf/bytes.o ../leaf/port.o
