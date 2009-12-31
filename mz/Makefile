MODULE=mz
SHLIB=libmz.so
SRC=mz.c

DEP_OBJ=$(BUILDDIR)/ex/libprim_ex.a $(BUILDDIR)/leaf/libprim_leaf.a 
# DEP_OBJ=$(BUILDDIR)/ex/*.o $(BUILDDIR)/leaf/*.o 

LIBS += -lpthread

all: $(SHLIB)

include ../Makefile.defs
include ../Makefile.rules

