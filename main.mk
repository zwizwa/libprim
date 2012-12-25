# Main makefile, alternative implementation to project.mk
# Simplified, according to these design goals:
#  (1) no recursive make[1]
#  (2) all build targets and intermediates go into separate build
#  (3) one platform per make invokation (parameterized)
#  (4) collect all code in a single .a to avoid .a order problems
#  (5) a build can have multiple output targets, which all link against the main .a
#  (6) shun phony targets ("make clean" should be just "rm -rf build")
#  (7) allow a simple "list of modules" approach

# Recursive make is not a good approach[1].  For multiple target
# platforms, it is simpler to parameterize the makefile over the
# target platform, instead of including multiple target rules in the
# main makefile.  Supporting simple individual target builts

# [1] http://miller.emu.id.au/pmiller/books/rmch/

# Disable all implicit rules
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# Both SRC and BUILD are absolute paths.
SRC      := $(shell readlink -f ..)
BUILD    := $(shell readlink -f .)

TARGET = Linux
# TOOL_PREFIX is empty
-include $(SRC)/target/$(TARGET).mk

CPPFLAGS := -I$(BUILD) -I$(SRC) -DTARGET=\"$(TARGET)\" -DPRIM_HOME=\"$(SRC)\"
CFLAGS   := -g -O0
LDFLAGS  :=
GCC      := $(TOOL_PREFIX)gcc
AS       := $(TOOL_PREFIX)as
AR       := $(TOOL_PREFIX)ar
LD       := $(TOOL_PREFIX)ld
CC       := $(GCC)
MZSCHEME := mzscheme

.PHONY: all
all: g_OUT

# Makefile template for the module.mk context. Once expanded, it
# gathers module-specific data from the local m_ variables and
# collects info in global g_ variables.  Note that '$' varrefs are
# expanded at template expansion-time, while '$$' varrefs survive as
# '$' refs in the expanded code.
define module_template
m_C        :=
m_O        :=
m_D        :=
m_H        :=
m_OUT      :=
m_SRC      := $(SRC)/$(1)
m_BUILD	   := $(BUILD)/$(1)
m_LDFLAGS  :=
include  $$(m_SRC)/module.mk
g_C        += $$(addprefix $$(m_SRC)/,$$(m_C))
g_O	   += $$(addprefix $$(m_BUILD)/,$$(m_C:.c=.o))
g_D        += $$(addprefix $$(m_BUILD)/,$$(m_C:.c=.d))
g_H	   += $$(addprefix $$(m_BUILD)/,$$(m_H))
g_OUT	   += $$(addprefix $$(m_BUILD)/,$$(m_OUT))
g_LDFLAGS  += $$(m_LDFLAGS)
endef

# Expand template for each module and include the dependency files.
# Note that initialization of the g_ accumulators is necessary to turn
# them into simply expanded variables, since the default is recursive
# expanding variables.
g_C        :=
g_O        :=
g_D        :=
g_H        := $(BUILD)/config.h
g_OUT      :=
g_LDFLAGS  := 
$(foreach prog,$(MODULES),$(eval $(call module_template,$(prog))))
-include $(g_D)

.PHONY: g_OUT
g_OUT: $(g_OUT)


# All build rules are shared for the project.  It is not worth the
# complexity to have sub-project dependent build flags.  If this is
# necessary, abstract the subproject in a library dependency.

# Remove the '@' character for more verbose compilation.
compile = @mkdir -p $(dir $(1)) ; echo [$(2)] $(notdir $(1)) ; $(3)

# Gather dependencies from .c file
depstx := sed -r 's,\s(\w+/), $(BUILD)/\1,g'
$(BUILD)/%.d: $(SRC)/%.c $(g_H)
	$(call compile,$@,d,$(CC) $(CPPFLAGS) -M -MG -MT $(@:.d=.o) $< | $(depstx) >$@)

# Preprocess only
$(BUILD)/%.E.c: $(SRC)/%.c $(g_H)
	$(call compile,$@,c,$(CC) $(CPPFLAGS) -E $< -o $@)

_CC := $(CC) $(CPPFLAGS) $(CFLAGS)

# Intermediate object
$(BUILD)/%.o: $(SRC)/%.c $(BUILD)/%.d
	$(call compile,$@,o,$(_CC) -o $@ -c $<)

# Archive all object files from sources listed in m_C variables.
$(BUILD)/lib.a: $(g_O)
	$(call compile,$@,a,$(AR) -r $@ $(g_O) 2>/dev/null)

# Executable
$(BUILD)/%.elf: $(BUILD)/%.o $(BUILD)/lib.a
	$(call compile,$@,elf,$(_CC) $< $(BUILD)/lib.a $(g_LDFLAGS) -o $@)

# Extract symbols
# $(BUILDDIR) -> $(BUILDDIR) rules can use simpler patterns.
$(BUILD)/%.syms: $(BUILD)/%.so
	$(call compile,$@,syms,$(OBJDUMP) -T $< | grep '\.text' | awk '{print $$7;}' >$@)

# Generated header files
$(BUILD)/%.g.h: $(SRC)/%.c
	$(call compile,$@,h,$(MZSCHEME) $(dir $<)gen_prims.ss $< $@)

$(BUILD)/config.h:
	$(call compile,$@,h,echo '#include <target/$(TARGET).h>' > $@)


