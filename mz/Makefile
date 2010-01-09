MODULE=mz
SHLIB=libmz.so
SRC=vm.c
APP=vm

DEP_OBJ=$(BUILDDIR)/ex/libprim_ex.a $(BUILDDIR)/leaf/libprim_leaf.a 
# DEP_OBJ=$(BUILDDIR)/ex/*.o $(BUILDDIR)/leaf/*.o 

LIBS += -lpthread

GEN = vm.h_prims

all: $(SHLIB) $(APP)

include ../Makefile.defs
include ../Makefile.rules

BOOT=$(BUILDDIR)/sc/boot12.gscm
$(BOOT):
	make -C $(BUILDDIR)/sc `basename $(BOOT)`

test: $(BOOT) vm
	gdb --args ./vm --boot $(BOOT)
