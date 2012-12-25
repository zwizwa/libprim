m_OUT      := main.elf
m_LDFLAGS  := -lpthread -ldl -lm

# Bootfile needs to be expanded using the VM.
SC_VM1_POSIX := $(m_BUILD)/main.elf
SC_VM1_POSIX_RUN := $(SC_VM1_POSIX) --boot $(SC_BOOT_SCM)
SC_BOOT_EXPANDED_SCM := $(m_BUILD)/boot-expanded.scm
$(SC_BOOT_EXPANDED_SCM): $(SC_BOOT_SCM) $(SC_BOOT_EXPAND_SCM) $(SC_VM1_POSIX)
	$(call compile,$@,scm,$(SC_VM1_POSIX_RUN) --eval '(script)' $(SC_BOOT_EXPAND_SCM) $< $@)

# Add these to the global targets manually.
# g_OUT += $(SC_BOOT_EXPANDED_SCM) $(SC_START)

