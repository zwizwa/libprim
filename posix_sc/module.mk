# posix_sx/
m_OBJ      := main.o
m_DEPS     := sc_vm1 sc ex_posix ex leaf_posix leaf
m_TARGET   := main
m_LDFLAGS  := -lpthread

# Test target.
SC_VM1_POSIX := $(m_BUILD)/main
SC_BOOT_SCM := $(m_SRC)/../sc_vm1/boot.scm
SC_BOOT_EXPAND_SCM := $(m_SRC)/../sc_vm1/boot_expand.scm
SC_VM1_POSIX_RUN := $(SC_VM1_POSIX) --boot $(SC_BOOT_SCM)

$(m_BUILD)/start: $(SC_VM1_POSIX)
	$(call build, echo $(SC_VM1_POSIX_RUN) >$@ && chmod +x $@)

.PHONY: posix_sc_gdb
posix_sc_gdb: $(SC_VM1_POSIX)
	# gdb -x $(SRCDIR)/bin/run.gdb --args $(SC_VM1_POSIX_RUN) 
	$(GDB) --args $(SC_VM1_POSIX_RUN) 


# For expanding boot file a bootstrap is necessary.  When
# cross-compiling you need to first compile for the host to run the
# expander, then compile for the target.

SC_BOOT_EXPANDED_SCM=$(m_BUILD)/boot-expanded.scm
$(SC_BOOT_EXPANDED_SCM): $(SC_BOOT_SCM) $(SC_BOOT_EXPAND_SCM) $(SC_VM1_POSIX)
	$(SC_VM1_POSIX_RUN) --eval '(script)' $(SC_BOOT_EXPAND_SCM) $< $@

.PHONY: posix_boote
posix_boote: $(SC_BOOT_EXPANDED_SCM)

