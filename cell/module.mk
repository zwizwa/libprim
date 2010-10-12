MODULE_OBJ := test.o read.o vm.o gc.o 

TARGET         := cell
TARGET_MODULES := leaf

CELL_BIN := $(BUILDDIR)/$(MODULE)/cell 
CELL_TEST := $(SRCDIR)/$(MODULE)/test123.sxp

.PHONY: cell123
cell123: $(CELL_BIN) $(CELL_TEST)
	$(CELL_BIN) <$(CELL_TEST)
