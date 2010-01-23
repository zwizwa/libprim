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

BOOT=$(BUILDDIR)/sc/boot12.scm_
$(BOOT):
	make -C $(BUILDDIR)/sc `basename $(BOOT)`

.PHONY: test
test: $(BOOT) vm lib.scm_
	gdb --args ./vm --boot $(BOOT)

LIBDEPS= \
	$(SRCDIR)/sc/boot1-expand-letrec.scm \
	$(SRCDIR)/sc/boot1-expand-let.scm \
	$(SRCDIR)/sc/boot1-expand-lambda.scm \
	$(SRCDIR)/sc/boot1-expand-define.scm

lib.scm_: $(LIBDEPS)
	cat $(LIBDEPS) >lib.scm_
