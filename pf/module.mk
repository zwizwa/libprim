# pf/
m_H	  := pf.g.h
m_OUT     := pf.elf
m_LDFLAGS := -lpthread -ldl -lm


PF:= $(m_BUILD)/pf
PF_BOOT := $(m_SRC)/boot.pf

$(m_BUILD)/start: $(PF)
	$(call build, echo  $(PF) --boot $(PF_BOOT) >$@; chmod +x $@)

pf_test: $(PF)
	gdb -x $(SRCDIR)/bin/run.gdb --args $(PF) --boot $(PF_BOOT)
