# Sources for $(LOCAL_MODULE).a
LOCAL_OBJ := vm1.o main.o

# Define a new global build target.
$(call app_rule,sc_vm1, sc ex leaf, -lpthread)

# Boot file.
SC_VM1 := $(LOCAL_MODULE)/sc_vm1
SRC_SC := $(SRCDIR)/sc
BOOT_SCM_DEPS := $(addprefix $(SRC_SC)/, \
	boot1.scm \
	boot1-expand-define.scm \
	boot1-expand-letrec.scm \
	boot1-expand-let.scm \
	boot1-expand-lambda.scm \
	boot2.scm)
BOOT_SCM := $(LOCAL_MODULE)/boot12.scm_
$(BOOT_SCM): $(BOOT_SCM_DEPS)
	$(call build, cd $(SRC_SC); $(MZSCHEME) bootstrap.ss >$(BUILDDIR)/$@)

# Test target.
.PHONY: sc_vm1_test
sc_vm1_test: $(SC_VM1) $(BOOT_SCM)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1) --boot $(BOOT_SCM)


