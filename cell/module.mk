# cell/
m_C      := read.c vm.c gc.c  # test.c
m_TARGET := cell

m_DEPS   := leaf


CELL_BIN := $(m_BUILD)/cell 
CELL_TEST := $(m_SRC)/test123.sxp

CELL_COMP := $(m_SRC)/comp.ss

.PHONY: cell123 cell

cell: $(CELL_BIN)

cell123: $(CELL_BIN) $(CELL_TEST)
	$(CELL_BIN) <$(CELL_TEST)

cell_comp: $(CELL_BIN) $(CELL_COMP)
	mzscheme $(CELL_COMP) | $(CELL_BIN)
