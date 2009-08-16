APP=pf
SRC=pf.c lib.c
DEP_OBJ=../leaf/leaf.a ../ex/ex.a

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

all: pf $(DEP_OBJ)

# generated files
%.h_pf_prims: %.c prims_pf.ss
	mzscheme prims_pf.ss $< >$@
%.h_px_prims: %.c prims_px.ss
	mzscheme prims_px.ss $< >$@
%.h_load: %.pf ../load.ss
	mzscheme ../load.ss $< >$@

GEN = pf.h_pf_prims pf.h_px_prims boot.h_load

include ../Makefile.rules
