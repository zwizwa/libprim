# Sources for $(LOCAL_MODULE).a
LOCAL_OBJ=vm2.o

# Define global application target.
$(call app_rule,sc_vm2, ex leaf, -lpthread)

# Boot file comes from VM1
SC_VM2       := $(LOCAL_MODULE)/sc_vm2
SC_VM2_BOOT  := $(SRCDIR)/sc_vm1/boot.scm

# The test is run in the source directory to have easy access to .scm
# files from the console.  (Since this rule depends on LOCAL_MODULE
# it's stored in a variable.)
SC_VM2_TEST_CMD := \
	cd $(SRCDIR)/$(LOCAL_MODULE); \
	gdb -x $(SRCDIR)/bin/run.gdb \
		--args $(BUILDDIR)/$(SC_VM2) \
		--boot $(SC_VM2_BOOT) \
		--eval '(begin (load "vm1vm2.scm") (repl))'

.PHONY: sc_vm2_test
sc_vm2_test: $(SC_VM2)
	$(SC_VM2_TEST_CMD)

