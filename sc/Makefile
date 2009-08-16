
APP=scheme
SRC=lib.c scheme.c main.c 
DEP_OBJ=../leaf/leaf.a ../ex/ex.a

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

all: scheme $(DEP_OBJ)

# generated files
%.h_load: %.scm ../load.ss
	mzscheme ../load.ss $< >$@
%.h_sc_prims: %.c prims_sc.ss
	mzscheme prims_sc.ss $< >$@

GEN = scheme.h_sc_prims boot.h_load test.h_load

include ../Makefile.rules
