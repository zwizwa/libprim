# This Makefile builds a standalone minimal C application for ARM
# boards.  Supported targets are in targets/*.h

# TARGET := at91sam7s256  ## needs __ASSEMBLY__ fix in Atmel header
TARGET := at91sam7x256
TOOL_PREFIX := arm-none-eabi-

# Shared code repository.  Apps will be linked against this.
CODE_C := os.c
CODE_S := crt0.S

# Apps to build.
APP_C := test.c


#---------------------------------------------------------------------------

# Place the build into a separate directory to allow invokation of
# this Makefile for different TARGET values.
BUILD := build/$(TARGET)

CODE_O  := $(patsubst %.c,$(BUILD)/%.o,$(CODE_C)) $(patsubst %.S,$(BUILD)/%.o,$(CODE_S))
APP_ELF := $(patsubst %.c,$(BUILD)/%.elf,$(APP_C))

.PHONY: all
all: $(APP_ELF)

GCC := $(TOOL_PREFIX)gcc
AS  := $(TOOL_PREFIX)as
AR  := $(TOOL_PREFIX)ar
LD  := $(TOOL_PREFIX)ld


CFLAGS += -O3 -fomit-frame-pointer -fdata-sections -ffunction-sections -Wall -DTARGET=\"$(TARGET)\" -DTARGET_H=\"target/$(TARGET).h\"

# GCC is tied to newlib with syscall holes (_write(), _read(), ...) so
# we can just use it's libc at the end of the linker command line.  We
# can't use it's start files, so those are loaded explicitly as the
# first .o on the linker command line.
LIBGCC := $(shell $(GCC) -print-file-name=libgcc.a)
LIBC   := $(shell $(GCC) -print-file-name=libc.a)

# The linker line: startfile, app library, libc, libc stubs, libgcc
LD_LIBS   := $(BUILD)/crt0.o $(BUILD)/code.a $(LIBC) $(BUILD)/stubs.a $(LIBGCC)
LD_SCRIPT := $(BUILD)/flash.ld
LD_DEPS  := $(LD_LIBS) $(LD_SCRIPT)
LDFLAGS  := -nostartfiles -T $(LD_SCRIPT) --gc-sections -g

# Remove the '@' character for more verbose compilation.
compile = mkdir -p $(dir $(1)) ; echo [$(2)] $(notdir $(1)) ; $(3)

.PHONY: clean
clean:
	rm -rf *~ build





$(BUILD)/%.o: %.S Makefile
	$(call compile,$@,o,$(GCC) $(CFLAGS) -g -c $< -o $@)

$(BUILD)/%.o: %.c Makefile
	$(call compile,$@,o,$(GCC) $(CFLAGS) -g -c $< -o $@)


# code.a contains all the application code, stubs.a only the libc
# stubs.  These are 2 separate libraries since they need to be on both
# sides of libc on the command line.
$(BUILD)/code.a: $(CODE_O)
	$(call compile,$@,a,$(AR) -r $@ $(CODE_O) 2>/dev/null)
$(BUILD)/stubs.a: $(BUILD)/stubs.o
	$(call compile,$@,a,$(AR) -r $@ $(BUILD)/stubs.o 2>/dev/null)


# Linker script is preprocessed: it includes target header file which has memory layout info.
# LDT_CFLAGS := -C   # For leaving in comments
$(BUILD)/%.ld: target/$(TARGET).h flash.ldt  
	$(call compile,$@,ld,$(GCC) $(LDT_CFLAGS) -DTARGET_H=\"target/$(TARGET).h\" -P -E --language=c flash.ldt -o $@)

# ELF binaries "pull" code out of code.a by referencing it.6
$(BUILD)/%.elf:	$(BUILD)/%.o $(LD_DEPS)
	$(call compile,$@,elf,$(LD) -Map $(BUILD)/$*.map $(LDFLAGS) $(BUILD)/$*.o $(LD_LIBS) -o $@)

# For debug.
$(BUILD)/%.s: %.c Makefile
	$(call compile,$@,s,$(GCC) $(CFLAGS) -S -c $< -o $@)
