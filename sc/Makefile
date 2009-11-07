
APP=sc
SRC=scheme.c main.c
# lib.c
DEP_OBJ=../leaf/libprim_leaf.a ../ex/libprim_ex.a

# media?
# LIBS+=`pkg-config libavformat --libs`
# DEP_OBJ+=../media/libprim_media.a
# CPPFLAGS += `pkg-config libavformat --cflags`

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

SC_LIB_OBJ = scheme.o
SC_APP_OBJ = $(SC_LIB_OBJ) main.o $(DEP_OBJ)


all: $(APP) libprim_sc.a

libprim_sc.a: $(SC_LIB_OBJ)
	ar rcs libprim_sc.a $(SC_LIB_OBJ)



# generated files
%.h_load: %.scm ../load.ss
	mzscheme ../load.ss $< >$@
%.h_sc_prims: %.c sc_prims.ss ../prim-tools.ss
	mzscheme sc_prims.ss $< >$@

GEN = scheme.h_sc_prims test.h_load media.h_sc_prims
# boot.h_load 

include ../Makefile.defs
include ../Makefile.rules

