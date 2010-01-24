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




all: _all

# Include build variables.
include Makefile.defs

# Include "module.mk" Makefile fragments from these subdirectories.
MODULES := test leaf ex sc sc_vm1 sc_vm2 pf

# Create build tree
$(shell mkdir -p $(MODULES))

# Quiet build messages.
build = $(if $(VERBOSE), $(1), @echo "$@"; $(1))

# Template rule for applications: app <target>, <deps>, <libs>
define app
APPS := $$(APPS) $(1)
$(1): $(2)
	$$(call build, $$(CC) -o $(1) $(2) $(3) $$(LDFLAGS) $$(APP_LDFLAGS))
endef

# Convenience macro for creating application build rules.  All such
# targets defined are collected in the APPS variable.

# app_rule: <target>, <libprim_modules>, <extra_deps>, <libs>

modules  = $(foreach m, $(1), $(m)/$(m).a)
app_rule = $(eval $(call app, $(1)/$(strip $(1)), $(2) $(call modules, $(3)), $(4)))

# These are bodies of build rules.  Note that these variables _need_
# delayed evaluation, as the $@ and $< variables are filled in when
# the rule is applied.


# The .c deps are created using gcc -M, ignoring generated files (-MG)
# and explicity prefixing the output rule with the directory of the
# object file.

rule_d_c = $(call build, (echo -n $(dir $@); $(CC) -M -MG $(CPPFLAGS) $<) >$@)
rule_o_c = $(call build, $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<)

# We grab the gen_prims.ss script next to the source .c file.
rule_h_prims_c = $(call build, $(MZSCHEME) $(dir $<)gen_prims.ss $< >$@)
rule_h_pf_prims_c = $(call build, $(MZSCHEME) $(dir $<)pf_prims.ss $< >$@)

rule_test_c = $(call build, $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) $< -o $@ $(LDFLAGS))


# This function generates the makefile fragment for each module.  Once
# expanded, it constructs a set of variables and build rules
# specialized to the module.
define module
# Define module name for use in included fragment + clear locals.
LOCAL_MODULE:=$(1)
LOCAL_OBJ :=
LOCAL_APP :=
LOCAL_MODULES :=
LOCAL_LDFLAGS :=
include $(SRCDIR)/$(1)/module.mk

# Localized build rules: grab sources in $(SRCDIR).
$(1)/%.d: $(SRCDIR)/$(1)/%.c 
	$$(rule_d_c)
$(1)/%.o: $(SRCDIR)/$(1)/%.c 
	$$(rule_o_c)
$(1)/%.h_prims: $(SRCDIR)/$(1)/%.c
	$$(rule_h_prims_c)
$(1)/%.h_pf_prims: $(SRCDIR)/$(1)/%.c
	$$(rule_h_pf_prims_c)
$(1)/%.test: $(SRCDIR)/$(1)/%.c
	$$(rule_test_c)

# Get list of sources and objects to construct the list of .d includes
# generated by gcc -M, and a list of objects to collect in the .a
# archive.

# The module.mk : LOCAL_OBJ variable lists the binary objects to be
# gathered in the $(LOCAL_MODULE).a archive.
$(1)_OBJ := $$(addprefix $(1)/, $$(LOCAL_OBJ))

# Each object .o should have a corresponding rule to generate a .d
# dependency file.
DEPS := $$(DEPS) $$($(1)_OBJ:.o=.d)

$(1)/$(1).a: $$($(1)_OBJ)
	@echo "$$@"
	@ar rcs $$@ $$($(1)_OBJ)

$$(if $$(LOCAL_APP), $$(call app_rule, \
	$$(LOCAL_APP), \
	$$($(1)_OBJ), \
	$$(LOCAL_APP_MODULES), \
	$$(LOCAL_APP_LDFLAGS)))

# Clear local vars.
endef

# Expand template for each module
$(foreach prog,$(MODULES),$(eval $(call module,$(prog))))

# Invalidate module-local vars.
LOCAL_OBJ    := out_of_scope_LOCAL_OBJ
LOCAL_MODULE := out_of_scope_LOCAL_MODULE

# Include generated dependencies.
-include $(DEPS)


.PHONY: all clean

ALL := $(APPS)

_all: $(ALL)

install: $(ALL)
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
