MODULE=mz
SHLIB=libmz.so
SRC=vm.c
APP=vm

DEP_OBJ=$(BUILDDIR)/ex/libprim_ex.a $(BUILDDIR)/leaf/libprim_leaf.a 
# DEP_OBJ=$(BUILDDIR)/ex/*.o $(BUILDDIR)/leaf/*.o 

LIBS += -lpthread

GEN = vm.h_prims

all: $(SHLIB)

include ../Makefile.defs
include ../Makefile.rules

BOOTE=$(BUILDDIR)/sc/boot-expanded.scm
$(BOOTE):
	make -C $(BUILDDIR)/sc boot-expanded.scm

test: $(BOOTE) vm
	gdb --args ./vm --boot $(BOOTE)