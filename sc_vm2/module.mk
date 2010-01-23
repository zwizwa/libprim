LOCAL_OBJ=vm2.o


# All global rules created inside a module.mk that use the MODULE
# variable need to expand it using ":=" before the end of the file.
BOOT:=sc_vm1/boot12.scm_
VM_TEST_DEPS := $(MODULE)/vm2 $(BOOT)

# The test is run in the source directory to have easy access to .scm
# files from the console.
VM_TEST_CMD := \
	cd $(SRCDIR)/$(MODULE); \
	gdb -x $(SRCDIR)/bin/run.gdb \
		--args $(BUILDDIR)/$(MODULE)/vm2 \
		--boot $(BUILDDIR)/$(BOOT) $(VM_TEST_EVAL) \
		--eval '(begin (load "macros.scm") (load "compile.scm") (repl))'

.PHONY: vm_test vm_all

sc_vm2_all: $(VM_TEST_DEPS)

sc_vm2_test: $(VM_TEST_DEPS)
	$(VM_TEST_CMD)

