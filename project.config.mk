## Project-specific setup
## Included from project.mk

# Some architecture-specific rules
.PHONY: eCos Posix Linux Darwin

# ecos: $(BUILDDIR)/ecos_sc/main
ecos: $(BUILDDIR)/ecos_test/main
Posix: $(BUILDDIR)/posix_sc/start $(BUILDDIR)/pf/start
Linux: Posix
Darwin: Posix

PATH := $(PATH_HEAD)$(PATH)

# FIXME: do this somewhere else
CFLAGS += -falign-functions=4


$(BUILDDIR)/%.h_prims: $(SRCDIR)/%.c
	$(call build, $(MZSCHEME) $(dir $<)gen_prims.ss $< $@)



# Wrap scheme files as binary objects.  In two steps: zero terminate
# and conversion to object file. 

# We try to set the architecture options from the name of the objcopy
# binary.  There is probably a better way to do this, cuz this is
# horrible.
OBJCOPY.arm-eabi-objcopy := -O elf32-littlearm -B arm
OBJCOPY.objcopy := -O elf32-i386 -B i386
OBJCOPY_ARGS := $(OBJCOPY.$(OBJCOPY))

$(BUILDDIR)/%.scm0: $(SRCDIR)/%.scm
	$(call build, cp $< $@; echo -ne '\000' >>$@)

# The "cd $(dir $<)" is to make sure objcopy generates a predictable
# symbol from the base file name, excluding directory.
$(BUILDDIR)/%.o: $(BUILDDIR)/%.scm0
	$(call build, cd $(dir $<); $(OBJCOPY) -I binary $(OBJCOPY_ARGS) --rename-section .data=.rodata $(notdir $<) $@)




install: $(TARGETS)
	install -d $(PREFIX)/lib/pkgconfig
	install -m 644 libprim.pc $(PREFIX)/lib/pkgconfig

	install -d $(PREFIX)/share/prim/
	install -m 644 $(SRCDIR)/sc*/*.scm $(PREFIX)/share/prim/
	install -m 644 $(SRCDIR)/pf/*.pf $(PREFIX)/share/prim/

	$(foreach dir, ex ex_posix leaf leaf_posix sc media, \
		install -d $(PREFIX)/include/prim/$(dir); \
		install -m 644 $(SRCDIR)/$(dir)/*.h* $(PREFIX)/include/prim/$(dir);)

## Don't install intermediate .a libs.
#	install -m 755 */libprim_*.a $(PREFIX)/lib/

	install -d $(PREFIX)/bin
	install -m 755 $(TARGETS) $(PREFIX)/bin

uninstall:
	rm -rf $(PREFIX)/share/prim
	rm -rf $(PREFIX)/include/prim
#	rm -rf $(PREFIX)/lib/libprim_*.a
	rm -rf $(PREFIX)/bin/sc
	rm -rf $(PREFIX)/bin/pf
