# Sources for $(LOCAL_MODULE).a
LOCAL_OBJ=vm2.o

# Define global application target.
SC_VM2 := $(LOCAL_MODULE)/vm2
$(call app_rule, $(SC_VM2), sc_vm2 sc ex leaf, -lpthread)

# Boot file comes from VM1
SC_VM2_BOOT := sc_vm1/boot12.scm_
VM_TEST_DEPS = $(SC_VM2) $(SC_VM2_BOOT)

# The test is run in the source directory to have easy access to .scm
# files from the console.  (Since this rule depends on LOCAL_MODULE
# it's stored in a variable.)
VM_TEST_CMD := \
	cd $(SRCDIR)/$(LOCAL_MODULE); \
	gdb -x $(SRCDIR)/bin/run.gdb \
		--args $(BUILDDIR)/$(SC_VM2) \
		--boot $(BUILDDIR)/$(SC_VM2_BOOT) \
		--eval '(begin (load "macros.scm") (load "compile.scm") (repl))'

.PHONY: sc_vm2_all sc_vm2_test
sc_vm2_all: $(VM_TEST_DEPS)

sc_vm2_test: $(VM_TEST_DEPS)
	$(VM_TEST_CMD)

