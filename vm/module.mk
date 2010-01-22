SRC=vm.c




# All global rules created inside a module.mk that use the MODULE
# variable need to expand it using ":=" before the end of the file.
BOOT:=sc/boot12.scm_
VM_TEST_DEPS := $(MODULE)/vm $(BOOT)

# The test is run in the source directory to have easy access to .scm
# files from the console.
VM_TEST_CMD := cd $(SRCDIR)/$(MODULE); \
	gdb --args $(BUILDDIR)/$(MODULE)/vm --boot $(BUILDDIR)/$(BOOT)

.PHONY: vm_test
vm_test: $(VM_TEST_DEPS)
	$(VM_TEST_CMD)

