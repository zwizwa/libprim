# Define objects in this module
MODULE_OBJ := main.o

# Define a new global build target.
TARGET         := main
TARGET_MODULES := sc_vm1 sc ex_posix ex leaf_posix leaf
TARGET_LDFLAGS := -lpthread

# Test target.


SC_VM1_POSIX := $(BUILDDIR)/$(MODULE)/main
SC_BOOT_SCM := $(SRCDIR)/sc_vm1/boot.scm
.PHONY: posix_sc
posix_sc: $(SC_VM1_POSIX)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1_POSIX) --boot $(SC_BOOT_SCM)


