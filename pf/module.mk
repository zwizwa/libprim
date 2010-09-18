MODULE_OBJ := pf.o 

# Application target: 
# Define a new global build target.
TARGET         := pf
TARGET_MODULES := px ex ex_posix leaf leaf_posix
TARGET_LDFLAGS := -lpthread

PF:= $(BUILDDIR)/$(MODULE)/pf
PF_BOOT := $(SRCDIR)/$(MODULE)/boot.pf

pf_test: $(PF)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(PF) --boot $(PF_BOOT)