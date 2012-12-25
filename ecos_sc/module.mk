ifeq ($(ARCH),ecos)

m_C       := main.c
m_TARGET  := main
m_DEPS    := sc_vm1 sc ex leaf

# Test target.
SC_VM1_ECOS := $(BUILDDIR)/$(m_NAME)/main

.PHONY: ecos_sc ecos_sc_gdb
ecos_sc_gdb: $(SC_VM1_ECOS)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1_ECOS)
ecos_sc: $(SC_VM1_ECOS)


endif
