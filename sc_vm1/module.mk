# Sources for $(LOCAL_MODULE).a
LOCAL_OBJ := vm1.o main.o

# Define a new global build target.
TARGET         := sc_vm1
TARGET_MODULES := sc ex leaf
TARGET_LDFLAGS := -lpthread

# Test target.
.PHONY: sc_vm1_test
SC_VM1 := $(LOCAL_MODULE)/sc_vm1
SC_BOOT_SCM := $(SRCDIR)/$(LOCAL_MODULE)/boot.scm
sc_vm1_test: $(SC_VM1)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1) --boot $(SC_BOOT_SCM)
