# Define objects in this module
MODULE_OBJ := main.o

# Define a new global build target.
TARGET         := main
TARGET_MODULES := sc_vm1 sc ex_posix ex leaf_posix leaf
TARGET_LDFLAGS := -lpthread

# Test target.
SC_VM1_POSIX := $(BUILDDIR)/$(MODULE)/main
SC_BOOT_SCM := $(SRCDIR)/sc_vm1/boot.scm
SC_VM1_POSIX_RUN := $(SC_VM1_POSIX) --boot $(SC_BOOT_SCM)
.PHONY: posix_sc
posix_sc: $(SC_VM1_POSIX)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1_POSIX_RUN) 


# For expanding boot file a bootstrap is necessary.  When
# cross-compiling you need to first compile for the host to run the
# expander, then compile for the target.

SC_BOOT_EXPANDED_SCM=$(BUILDDIR)/$(MODULE)/boot-expanded.scm
SC_BOOT_EXPANDED_SCM: $(SC_BOOT_SCM)
	$(SC_VM1_POSIX_RUN) --eval '(begin (load "$@")(script))' $< $@

,PHONY: posix_boote
posix_boote: $(SC_BOOT_EXPANDED_SCM)

