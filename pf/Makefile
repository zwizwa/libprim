include ../Makefile.defs

MODULE=pf
APP=pf
SRC=pf.c px.c
DEP_OBJ=$(BUILDDIR)/ex/libprim_ex.a $(BUILDDIR)/leaf/libprim_leaf.a 

LIBS += -lpthread

all: pf $(DEP_OBJ)

# generated files

SD = $(SRCDIR)/$(MODULE)

%.h_pf_prims: $(SD)/%.c $(SD)/pf_prims.ss $(SRCDIR)/prim-tools.ss
	$(MZSCHEME) $(SD)/pf_prims.ss $< >$@
%.h_px_prims: $(SD)/%.c $(SD)/px_prims.ss $(SRCDIR)/prim-tools.ss
	$(MZSCHEME) $(SD)/px_prims.ss $< >$@
%.h_load: $(SD)/%.pf $(SRCDIR)/load.ss
	$(MZSCHEME) $(SRCDIR)/load.ss $< >$@

GEN = pf.h_pf_prims px.h_px_prims
# boot.h_load

include ../Makefile.rules
