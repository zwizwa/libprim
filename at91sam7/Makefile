# This Makefile builds a standalone minimal C application for the
# Atmel AT91SAM7S-EK evaluation board.  FIXME: Later integrate with
# libprim build.

TARGET := at91sam7s256
TOOLPREFIX := /opt/xc/gnutools/bin/arm-none-eabi-

# Shared code repository.  Apps will be linked against this.
CODE_C := os.c

# Apps to build.
APP_C := test.c

BUILD := build
CODE_O := $(patsubst %.c,$(BUILD)/%.o,$(CODE_C))

APP_ELF := $(patsubst %.c,$(BUILD)/%.elf,$(APP_C))

.PHONY: all
all: $(APP_ELF)





GCC := $(TOOLPREFIX)gcc


CFLAGS += -O3 -fomit-frame-pointer -fdata-sections -ffunction-sections -Wall -DTARGET=$(TARGET)

# GCC is tied to newlib with syscall holes (_write(), _read(), ...) so
# we can just use it's libc at the end of the linker command line.  We
# can't use it's start files, so those are loaded explicitly as the
# first .o on the linker command line.
LIBGCC := $(shell $(GCC) -print-file-name=libgcc.a)
LIBC   := $(shell $(GCC) -print-file-name=libc.a)

# The linker line: startfile, app library, libc, libc stubs, libgcc
LD_START := $(BUILD)/crt0.o
LD_LIBS  := $(BUILD)/code.a $(LIBC) $(BUILD)/stubs.a $(LIBGCC)
LDFLAGS  := -nostartfiles -T $(TARGET).ld --gc-sections -g

compile = @mkdir -p $(dir $(1)) ; echo [$(2)] $(notdir $(1)) ; $(3)

.PHONY: clean
clean:
	rm -rf *~ build

$(BUILD)/%.o: %.s Makefile
	$(call compile,$@,o,$(TOOLPREFIX)as -g $< -o $@)

$(BUILD)/%.o: %.c Makefile
	$(call compile,$@,o,$(TOOLPREFIX)gcc $(CFLAGS) -g -c $< -o $@)


# code.a contains all the application code, stubs.a only the libc
# stubs.  These are 2 separate libraries since they need to be on both
# sides of libc on the command line.
$(BUILD)/code.a: $(CODE_O)
	$(call compile,$@,a,$(TOOLPREFIX)ar -r $@ $(CODE_O) 2>/dev/null)
$(BUILD)/stubs.a: $(BUILD)/stubs.o
	$(call compile,$@,a,$(TOOLPREFIX)ar -r $@ $(BUILD)/stubs.o 2>/dev/null)

# ELF binaries "pull" code out of code.a by referencing it.
$(BUILD)/%.elf:	$(BUILD)/%.o $(LD_START) $(LD_LIBS) $(TARGET).ld
	$(call compile,$@,elf,$(TOOLPREFIX)ld -Map $(BUILD)/$*.map $(LD_START) $(BUILD)/$*.o $(LD_LIBS) $(LDFLAGS) -o $@)

# For debug.
$(BUILD)/%.s: %.c Makefile
	$(call compile,$@,s,$(TOOLPREFIX)gcc $(CFLAGS) -S -c $< -o $@)
