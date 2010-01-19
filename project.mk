
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

# VPATH = $(SRCDIR)
# vpath %.c $(SRCDIR)


all: _all

MODULES := leaf ex sc pf

VOID := $(shell mkdir -p $(MODULES))

CFLAGS += $(pathsubst %, -I%, $(MODULES))

# Make sure the entire build depends on all makefile fragments.
MAKEFILES := project.mk $(pathsubst %, %/module.mk, $(MODULES))

%.d: %.c $(MAKEFILES)
	@echo "$@"
	@( echo -n "`dirname $<`/"; $(CC) -M -MG $(CPPFLAGS) $< ) > $@


%.o: %.c $(MAKEFILES)
	@echo "$@"
	@$(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<




# Generated header files.  These assume the .c file's directory
# contains the `gen_prims.ss' script.
%.h_prims: %.c
	@echo "$@"
	@$(MZSCHEME) `dirname $<`/gen_prims.ss $< >$@

%.h_pf_prims: %.c
	@echo "$@"
	@$(MZSCHEME) `dirname $<`/pf_prims.ss $< >$@




# This function generates a makefile fragment that gathers variables
# and build rules through an include, and accumulates them to global
# variables.
define module
MODULE := $(1) 	# Define module name for use in included fragment
include $$(SRCDIR)/$(1)/module.mk

# Get properly prefixed list of sources and objects.
$(1)_SRC := $$(patsubst %, $(1)/%, $$(SRC))
$(1)_OBJ := $$($(1)_SRC:.c=.o)
DEPS := $$(DEPS) $$($(1)_SRC:.c=.d)
# Build a single archive per module.
$(1)/$(1).a: $$($(1)_OBJ)
	@echo "$$@"
	@ar rcs $$@ $$($(1)_OBJ)
PROJECT_A := $$(PROJECT_A) $(1)/$(1).a
# debug print
# DUMMY := $$(shell echo $(1) : $$($(1)_SRC) >&2)

# Clear local vars.
SRC :=
MODULE :=
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
$(eval $(call app, sc/sc, sc/sc.a ex/ex.a leaf/leaf.a, -lm -lpthread))
$(eval $(call app, pf/pf, pf/pf.a ex/ex.a leaf/leaf.a, -lm -lpthread))


.PHONY: all clean

# OBJECTS := $(PROJECT_SRC:.c=.o)
OBJECTS := $(PROJECT_A)

_all: $(OBJECTS)

