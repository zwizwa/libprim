
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

include Makefile.defs

MODULES := leaf ex sc


CFLAGS += $(pathsubst %, -I%, $(MODULES))

# Make sure the entire build depends on all makefile fragments.
MAKEFILES := Project.mk $(pathsubst %, %/module.mk, $(MODULES)

%.d: %.c $(MAKEFILES) $(GEN)
	@echo "$@"
	@(set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$)

%.o: %.c $(MAKEFILES)
	@echo "$@"
	@$(CC) $(CPPFLAGS) $(CFLAGS) -o $@ -c $<

# Build makfile by including all makefile fragments.
define do_include
MODULE := $(1)
include $(1)/module.mk
PROJECT_SRC := $(PROJECT_SRC) $$(patsubst %, $(1)/%, $$(SRC))
# DUMMY := $$(shell echo $(1) : $$(SRC) >&2)
SRC :=
endef
$(foreach prog,$(MODULES),$(eval $(call do_include,$(prog))))

# Include generated dependencies.
-include $(PROJECT_SRC:.c=.d)

all: $(PROJECT_SRC:.c=.o)

