# Sources for $(MODULE).a
MODULE_OBJ=vm2.o

# Define a new global build target.
TARGET         := sc_vm2
TARGET_MODULES := sc ex leaf
TARGET_LDFLAGS := -lpthread

# Boot file comes from VM1
SC_VM2       := $(MODULE)/sc_vm2
SC_VM2_BOOT  := $(SRCDIR)/sc_vm1/boot.scm

# The test is run in the source directory to have easy access to .scm
# files from the console.  (Since this rule depends on MODULE
# it's stored in a variable.)
SC_VM2_TEST_CMD := \
	cd $(SRCDIR)/$(MODULE); \
	gdb -x $(SRCDIR)/bin/run.gdb \
		--args $(BUILDDIR)/$(SC_VM2) \
		--boot $(SC_VM2_BOOT) \
		--eval '(begin (load "vm1vm2.scm") (repl))'

.PHONY: sc_vm2_test

sv_vm2_all: $(SC_VM2)

sc_vm2_test: $(SC_VM2)
	$(SC_VM2_TEST_CMD)

