# pf/
m_c       := pf.c
m_TARGET  := pf
m_DEPS    := px ex ex_posix leaf leaf_posix
m_LDFLAGS := -lpthread

PF:= $(m_BUILD)/pf
PF_BOOT := $(m_SRC)/boot.pf

$(m_BUILD)/start: $(PF)
	$(call build, echo  $(PF) --boot $(PF_BOOT) >$@; chmod +x $@)

pf_test: $(PF)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(PF) --boot $(PF_BOOT)
