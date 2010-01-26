# Define objects in this module
MODULE_OBJ := vm3.o main.o

# Define a new global build target.
TARGET         := sc_vm3
TARGET_MODULES := sc ex leaf
TARGET_LDFLAGS := -lpthread

# Test target.
.PHONY: sc_vm3_test
SC_VM3 := $(MODULE)/sc_vm3
SC_BOOT_SCM := $(SRCDIR)/sc_vm1/boot.scm
sc_vm3_test: $(SC_VM3)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM3) --boot $(SC_BOOT_SCM)
