# Define objects in this module
MODULE_OBJ := vm1.o main.o boot.o

# Define a new global build target.
TARGET         := sc_vm1
TARGET_MODULES := sc ex ex_posix leaf leaf_posix
TARGET_LDFLAGS := -lpthread

# Test target.
.PHONY: sc_vm1_test
SC_VM1 := $(BUILDDIR)/$(MODULE)/sc_vm1
SC_BOOT_SCM := $(SRCDIR)/$(MODULE)/boot.scm
sc_vm1_test: $(SC_VM1)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1) --boot $(SC_BOOT_SCM)

sc_vm1_all: $(SC_VM1)
