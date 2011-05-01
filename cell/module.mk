MODULE_OBJ := test.o read.o vm.o gc.o 

TARGET         := cell
TARGET_MODULES := leaf

CELL_BIN := $(BUILDDIR)/$(MODULE)/cell 
CELL_TEST := $(SRCDIR)/$(MODULE)/test123.sxp

CELL_COMP := $(SRCDIR)/$(MODULE)/comp.ss

.PHONY: cell123 cell

cell: $(CELL_BIN)

cell123: $(CELL_BIN) $(CELL_TEST)
	$(CELL_BIN) <$(CELL_TEST)

cell_comp: $(CELL_BIN) $(CELL_COMP)
	mzscheme $(CELL_COMP) | $(CELL_BIN)
