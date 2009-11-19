APP=pf
SRC=pf.c px.c
DEP_OBJ=../ex/libprim_ex.a ../leaf/libprim_leaf.a 

LIBS += -lpthread

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

all: pf $(DEP_OBJ)

# generated files
%.h_pf_prims: %.c pf_prims.ss ../prim-tools.ss
	$(MZSCHEME) pf_prims.ss $< >$@
%.h_px_prims: %.c px_prims.ss ../prim-tools.ss
	$(MZSCHEME) px_prims.ss $< >$@
%.h_load: %.pf ../load.ss
	$(MZSCHEME) ../load.ss $< >$@

GEN = pf.h_pf_prims px.h_px_prims
# boot.h_load

include ../Makefile.defs
include ../Makefile.rules
