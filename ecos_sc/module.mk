# Define objects in this module
MODULE_OBJ := main.o

# Define a new global build target.
TARGET         := main
TARGET_MODULES := sc_vm1 sc ex leaf

# Test target.
SC_VM1_ECOS := $(BUILDDIR)/$(MODULE)/main

.PHONY: ecos_sc ecos_sc_gdb
ecos_sc_gdb: $(SC_VM1_ECOS)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1_ECOS)
ecos_sc: $(SC_VM1_ECOS)


