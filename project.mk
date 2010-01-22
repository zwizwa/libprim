
# Non-recursive make.  See:
#  [1] http://miller.emu.id.au/pmiller/books/rmch/
#  [2] http://www.xs4all.nl/~evbergen/nonrecursive-make.html
#  [3] http://make.mad-scientist.us/multi-arch.html

# The key point in making decent GNU makefiles seems to be
# understanding how substitution works; i.e. when exactly computations
# happen.  Original make was fully lazy functional, but GNU make has
# some strict (imperative) features that are useful as a macro
# facility.

# The whole project tree is managed by a single make session,
# constructed using file inclusion and the imperative (strict)
# features of make: assignment using ":=" instead of "=".  See section
# 5.2 in [1].

# To support separate build dirs, we use VPATH.

# Defines generated by the configure script.
# Makefile.defs: configure
#	./configure


include Makefile.defs

all: _all




MODULES := test leaf ex sc pf vm

# Strict variable assignment can be used to add imperative code to a
# Makefile.  The command will be executed when this line is parsed.
VOID := $(shell mkdir -p $(MODULES))

# CFLAGS += $(pathsubst %, -I%, $(MODULES))
CFLAGS += -I$(BUILDDIR) -I$(SRCDIR)

# These are bodies of build rules.  Note that these variables _need_
# delayed evaluation, as the $@ and $< variables are filled in when
# the rule is applied.

build = $(if $(VERBOSE), $(1), @echo "$@"; $(1))

# The .c deps are created using gcc -M, ignoring generated files (-MG)
# and explicity prefixing the output rule with the directory of the
# object file.

rule_d_c = $(call build, (echo -n $(dir $@); $(CC) -M -MG $(CPPFLAGS) $<) >$@)
rule_o_c = $(call build, $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<)

# We grab the gen_prims.ss script next to the source .c file.
rule_h_prims_c = $(call build, $(MZSCHEME) $(dir $<)gen_prims.ss $< >$@)
rule_h_pf_prims_c = $(call build, $(MZSCHEME) $(dir $<)pf_prims.ss $< >$@)

rule_test_c = $(call build, $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) $< -o $@ $(LIBS))

# This function generates the makefile fragment for each module.  Once
# expanded, it constructs a set of variables and build rules
# specialized to the module.
define module
MODULE:=$(1)
# Define module name for use in included fragment
include $(SRCDIR)/$(1)/module.mk

# Personalized build rules: grab sources in $(SRCDIR).
$(1)/%.d: $(SRCDIR)/$(1)/%.c 
	$$(rule_d_c)
# $(1)/%.o: $(1)/%.c
# 	$$(rule_o_c)
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
$(1)_SRC := $$(addprefix $(1)/, $$(SRC))
$(1)_OBJ := $$($(1)_SRC:.c=.o)
DEPS := $$(DEPS) $$($(1)_SRC:.c=.d)
$(1)/$(1).a: $$($(1)_OBJ)
	@echo "$$@"
	@ar rcs $$@ $$($(1)_OBJ)
PROJECT_A := $$(PROJECT_A) $(1)/$(1).a

# Clear local vars.
SRC    := undefined_SRC
MODULE := undefined_MODULE
endef

# Expand template for each module
$(foreach prog,$(MODULES),$(eval $(call module,$(prog))))

# Include generated dependencies.
-include $(DEPS)


# Products: app <target> <deps> <libs>
define app
$(1): $(2)
	@echo $(1)
	@$$(CC) $$(LDFLAGS) -o $(1) $(2) $(3)
endef

# Expand template for each app
BASE_A := ex/ex.a leaf/leaf.a
APP_LD := $(LIBS) $(APP_LDFLAGS) $(LDFLAGS) -lm -lpthread
$(eval $(call app, sc/sc, sc/sc.a $(BASE_A), $(APP_LD)))
$(eval $(call app, pf/pf, pf/pf.a $(BASE_A), $(APP_LD)))
$(eval $(call app, vm/vm, vm/vm.a $(BASE_A), $(APP_LD)))

.PHONY: all clean

# OBJECTS := $(PROJECT_SRC:.c=.o)
OBJECTS := $(PROJECT_A)
APPS := sc/sc vm/vm pf/pf sc/boot12.scm_

ALL := $(OBJECTS) $(APPS)
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
