# Input to global build system.  Sources for $(MODULE).a
SRC := sc.c scheme.c main.c

# Special cases
SRC_SC := $(SRCDIR)/$(MODULE)

BOOT_SCM_DEPS := $(addprefix $(SRC_SC)/, \
	boot1.scm \
	boot1-expand-define.scm \
	boot1-expand-letrec.scm \
	boot1-expand-let.scm \
	boot1-expand-lambda.scm \
	boot2.scm)

BOOT_SCM := $(MODULE)/boot12.scm_
$(BOOT_SCM): $(BOOT_SCM_DEPS)
	@echo $@; $(MZSCHEME) $(SRC_SC)/bootstrap.ss >$@

# VOID := $(shell echo $(BOOT_SCM) >&2)