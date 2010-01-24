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




all: targets

# Include build variables.
include Makefile.defs



# Include "module.mk" Makefile fragments from these subdirectories.
# MODULES := test leaf ex sc sc_vm1 sc_vm2 pf


MKS :=  $(shell cd $(SRCDIR); echo */*.mk)
MODULES := $(patsubst %/module.mk,%,$(MKS))

# $(shell echo $(MKS) >&2)


# Create build tree
$(shell mkdir -p $(MODULES))

# Quiet build messages.
build = $(if $(VERBOSE), $(1), @echo "$(patsubst $(BUILDDIR)/%,%,$@)"; $(1))

# Template rule for applications: app <target>, <deps>, <libs>
define target
TARGETS := $$(TARGETS) $(1)
$(1): $(2)
	$$(call build, $$(CC) -o $(1) $(2) $(3) $$(LDFLAGS) $$(APP_LDFLAGS))
endef

# Convenience macro for creating application build rules.  All such
# targets defined are collected in the APPS variable.

# target_rule: <module>, <target>, <objects>, <extra_deps>, <ldflags>

modules     = $(foreach m, $(1), $(BUILDDIR)/$(m)/$(m).a)
target_rule = $(eval $(call target, $(1)/$(strip $(2)), $(3) $(call modules, $(4)), $(5)))

# These are bodies of build rules.  Note that these variables _need_
# delayed evaluation, as the $@ and $< variables are filled in when
# the rule is applied.




_CC := $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS)

# The .c deps are created using gcc -M, ignoring generated files (-MG)
# and explicity prefixing the output rule with the directory of the
# object file.  The sed line prefixes all relative paths with
# $(BUILDDIR).

depstx := sed -r 's,\s(\w+/), $(BUILDDIR)/\1,g'
rule_d_c    = $(call build, $(CC) $(CPPFLAGS) -M -MG -MT $(@:.d=.o) $< | $(depstx) >$@)
rule_o_c    = $(call build, $(_CC) -o $@ -c $<)
rule_test_c = $(call build, $(_CC) $< -o $@ $(LDFLAGS))

# We grab the gen_prims.ss script next to the source .c file.
rule_h_prims_c    = $(call build, $(MZSCHEME) $(dir $<)gen_prims.ss $< >$@)
rule_h_pf_prims_c = $(call build, $(MZSCHEME) $(dir $<)pf_prims.ss $< >$@)


# All build rules use absolute paths.
$(BUILDDIR)/%.o : $(SRCDIR)/%.c 
	$(call build, $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<)

# Get list of sources and objects to construct the list of .d includes
# generated by gcc -M, and a list of objects to collect in the .a
# archive.

$(BUILDDIR)/%.d: $(SRCDIR)/%.c 
	$(rule_d_c)
$(BUILDDIR)/%.o: $(SRCDIR)/%.c 
	$(rule_o_c)
$(BUILDDIR)/%.h_prims: $(SRCDIR)/%.c
	$(rule_h_prims_c)
$(BUILDDIR)/%.h_pf_prims: $(SRCDIR)/%.c
	$(rule_h_pf_prims_c)
$(BUILDDIR)/%.test: $(SRCDIR)/%.c
	$(rule_test_c)


# This function generates the makefile fragment for each module.  Once
# expanded, it constructs a set of variables and build rules
# specialized to the module.
define module
# Define module name for use in included fragment + clear locals.
MODULE := $(1)
MODULE_OBJ :=
TARGET :=
TARGET_MODULES :=
TARGET_LDFLAGS :=
include $(SRCDIR)/$(1)/module.mk


# The module.mk : MODULE_OBJ variable lists the binary objects to be
# gathered in the $(MODULE).a archive.
$(1)_OBJ := $$(addprefix $(BUILDDIR)/$(1)/, $$(MODULE_OBJ))

# Each object .o should have a corresponding rule to generate a .d
# dependency file.
DEPS := $$(DEPS) $$($(1)_OBJ:.o=.d)

$(BUILDDIR)/$(1)/$(1).a: $$($(1)_OBJ)
	$$(call build, ar rcs $$@ $$($(1)_OBJ))

$$(if $$(TARGET), $$(call target_rule, \
	$(1), \
	$$(TARGET), \
	$$($(1)_OBJ), \
	$$(TARGET_MODULES), \
	$$(TARGET_LDFLAGS)))

# Clear local vars.
endef

# Expand template for each module
$(foreach prog,$(MODULES),$(eval $(call module,$(prog))))

# Include generated dependencies.
-include $(DEPS)


.PHONY: all clean targets

targets: $(TARGETS)

install: $(TARGETS)
	install -d $(PREFIX)/lib/pkgconfig
	install -m 644 libprim.pc $(PREFIX)/lib/pkgconfig

	install -d $(PREFIX)/share/prim/
	install -m 644 $(SRCDIR)/sc/*.scm $(PREFIX)/share/prim/
	install -m 644 $(BUILDDIR)/sc/boot12.scm_ $(PREFIX)/share/prim/boot.scm
	install -m 644 $(SRCDIR)/pf/*.pf $(PREFIX)/share/prim/

	$(foreach dir, ex leaf sc media, \
		install -d $(PREFIX)/include/prim/$(dir); \
		install -m 644 $(SRCDIR)/$(dir)/*.h* $(PREFIX)/include/prim/$(dir);)

## Don't install intermediate .a libs.
#	install -m 755 */libprim_*.a $(PREFIX)/lib/

	install -d $(PREFIX)/bin
	install -m 755 sc/sc $(PREFIX)/bin
	install -m 755 pf/pf $(PREFIX)/bin

uninstall:
	rm -rf $(PREFIX)/share/prim
	rm -rf $(PREFIX)/include/prim
#	rm -rf $(PREFIX)/lib/libprim_*.a
	rm -rf $(PREFIX)/bin/sc
	rm -rf $(PREFIX)/bin/pf


clean:
	rm -rf $(MODULES)


%.syms: %.so
	$(call build, objdump -T $< | grep '\.text' | awk '{print $$7;}' >$@)
