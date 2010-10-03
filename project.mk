# Non-recursive make.  See:
#  [1] http://miller.emu.id.au/pmiller/books/rmch/
#  [2] http://www.xs4all.nl/~evbergen/nonrecursive-make.html
#  [3] http://make.mad-scientist.us/multi-arch.html
#
# The key point in making decent GNU makefiles seems to be
# understanding how substitution works; i.e. at what time variables
# expansions are performed.  Original make is fully lazy functional,
# but GNU make has some strict (imperative) features that are useful
# as a macro facility.
#
# The whole project tree is managed by a single make session,
# constructed using file inclusion and the imperative (strict)
# features of make: assignment using ":=" instead of "=".  See section
# 5.2 in [1].
#
# We use functions and macros for conciseness in many places.  Note
# that GNU make is very similar to Scheme[4]. 
#
#   ($call body, args...)      
# 
# Function application: subsitute arguments in body and expand the
# resulting string.
#
#   ($eval $(call body, args)) 
#
# Macro expansion: generate code using function application and
# interpret the result as makefile syntax.  This is useful for
# creating rules.
#
#
#  [4] http://okmij.org/ftp/Computation/#Makefile-functional



# .SUFFIXES: .scm
.PHONY: all clean targets
.PHONY: eCos Posix Linux Darwin

# Include build variables.
include Makefile.defs

# all: targets
all: $(PLATFORM)


# FIXME: do this somewhere else
CFLAGS += -falign-functions=4

# Setup environment
B := $(BUILDDIR)
S := $(SRCDIR)
build = $(if $(VERBOSE), $(1), @echo "$(patsubst $(B)/%,%,$@)"; $(1))
MKS :=  $(shell cd $(S); echo */*.mk)
MODULES := $(patsubst %/module.mk,%,$(MKS))
$(shell mkdir -p $(MODULES))

# Build target specific rules
eCos: $(B)/ecos_sc/main
Posix: $(B)/posix_sc/start $(B)/pf/start
Linux: Posix
Darwin: Posix




### BUILD RULES

# All file names use absolute paths.

# Get list of sources and objects to construct the list of .d includes
# generated by gcc -M, and a list of objects to collect in the .a
# archive.

# The .c deps are created using gcc -M, ignoring generated files (-MG)
# and explicity prefixing the output rule with the directory of the
# object file.  The sed line prefixes all relative paths with
# $(B).

depstx := sed -r 's,\s(\w+/), $(B)/\1,g'
$(B)/%.d: $(S)/%.c 
	$(call build, $(CC) $(CPPFLAGS) -M -MG -MT $(@:.d=.o) $< | $(depstx) >$@)
$(B)/%.cx: $(S)/%.c
	$(call build, $(CC) $(CPPFLAGS) -E $< -o $@)

_CC := $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS)

$(B)/%.o: $(S)/%.c 
	$(call build, $(_CC) -o $@ -c $<)
$(B)/%.test: $(S)/%.c
	$(call build, $(_CC) $(LDFLAGS) $< -o $@)

$(B)/%.h_prims: $(S)/%.c
	$(call build, $(MZSCHEME) $(dir $<)gen_prims.ss $< $@)

# $(B) -> $(B) rules can use simpler patterns.
%.syms: %.so
	$(call build, objdump -T $< | grep '\.text' | awk '{print $$7;}' >$@)

# Wrap scheme files as binary objects.  In two steps: zero terminate
# and conversion to object file.  The "cd $(dir $<)" is to make sure
# objcopy generates a predictable symbol from the base file name,
# excluding directory.
$(B)/%.scm0: $(S)/%.scm
	$(call build, cp $< $@; echo -ne '\000' >>$@)
$(B)/%.o: $(B)/%.scm0
	$(call build, cd $(dir $<); $(OBJCOPY) -I binary $(OBJCOPY_ARCH) --rename-section .data=.rodata $(notdir $<) $@)


### MODULES

# Template rule and macro function for applications and shared library
# targets: <target>, <deps.a>, <ldflags>
define target_template
TARGETS := $$(TARGETS) $(1)
$(1): $(2)
	$$(call build, $$(CC) -o $(1) $(2) $(3) $$(LDFLAGS) $$(APP_LDFLAGS))
endef

# target_rule: <module>, <target>, <objects>, <extra_deps>, <ldflags>
modules     = $(foreach m, $(1), $(B)/$(m)/$(m).a)
target_rule = $(eval $(call target_template, $(1)/$(strip $(2)), \
		$(3) $(call modules, $(4)), $(5)))


# Makefile tiemplate for each module.  Once expanded, it gathers
# module-specific data from temporary variables and builds a global
# target list.
define module_template
MODULE         := $(1)
MODULE_OBJ     :=
TARGET         :=
TARGET_MODULES :=
TARGET_LDFLAGS :=
include $(S)/$(1)/module.mk

# Convert all objects to absolute paths.
$(1)_OBJ := $$(addprefix $(B)/$(1)/, $$(MODULE_OBJ))

# Each object has a dependency file.
DEPS := $$(DEPS) $$($(1)_OBJ:.o=.d)

# Each module is bundled in an .a archive.
$(B)/$(1)/$(1).a: $$($(1)_OBJ)
	$$(call build, $(AR) rcs $$@ $$($(1)_OBJ))

# Create rule for target if defined.
$$(if $$(TARGET), $$(call target_rule, \
	$(B)/$(1), \
	$$(TARGET), \
	$$($(1)_OBJ), \
	$$(TARGET_MODULES), \
	$$(TARGET_LDFLAGS)))
endef

# Expand template for each module, include deps and define targets.
$(foreach prog,$(MODULES),$(eval $(call module_template,$(prog))))
-include $(DEPS)
targets: $(TARGETS)



### INSTALL & CLEAN

install: $(TARGETS)
	install -d $(PREFIX)/lib/pkgconfig
	install -m 644 libprim.pc $(PREFIX)/lib/pkgconfig

	install -d $(PREFIX)/share/prim/
	install -m 644 $(S)/sc*/*.scm $(PREFIX)/share/prim/
	install -m 644 $(S)/pf/*.pf $(PREFIX)/share/prim/

	$(foreach dir, ex ex_posix leaf leaf_posix sc media, \
		install -d $(PREFIX)/include/prim/$(dir); \
		install -m 644 $(S)/$(dir)/*.h* $(PREFIX)/include/prim/$(dir);)

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

clean:
	cd $(B); rm -rf $(MODULES)
	cd $(S); rm -f `find -name '*~'`


