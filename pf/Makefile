APP=pf
SRC=pf.c px.c
DEP_OBJ=../leaf/leaf.a ../ex/ex.a

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

all: pf $(DEP_OBJ)

# generated files
%.h_pf_prims: %.c pf_prims.ss ../prim-tools.ss
	mzscheme pf_prims.ss $< >$@
%.h_px_prims: %.c px_prims.ss ../prim-tools.ss
	mzscheme px_prims.ss $< >$@
%.h_ex_prims: %.c ../ex/ex_prims.ss ../prim-tools.ss
	mzscheme ../ex/ex_prims.ss $< >$@
%.h_load: %.pf ../load.ss
	mzscheme ../load.ss $< >$@

GEN = pf.h_pf_prims px.h_px_prims boot.h_load

include ../Makefile.rules
