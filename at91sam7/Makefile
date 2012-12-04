# This Makefile builds a standalone minimal C application for the
# Atmel AT91SAM7S-EK evaluation board.  FIXME: Later integrate with
# libprim build.

C := os.c newlib_stubs.c
TARGET := at91sam7s256

BUILD := build
all: $(BUILD)/$(TARGET).elf

PLATFORM := arm-eabi
PFX := $(PLATFORM)-
LIBGCC := /opt/xc/arm-eabi/lib/gcc/arm-eabi/4.3.2/libgcc.a

# newlib: optional?
NEWLIB := newlib/newlib-1.18.0
NEWLIB_CFLAGS := -I$(NEWLIB)/newlib/libc/include
NEWLIB_LIBC_A := $(NEWLIB)/$(PLATFORM)/newlib/libc.a
NEWLIB_LDFLAGS := $(NEWLIB_LIBC_A)
CFLAGS  += $(NEWLIB_CFLAGS)
LDFLAGS += $(NEWLIB_LDFLAGS) 
newlib_info: $(NEWLIB_LIBC_A)
	@ls -l $(NEWLIB_LIBC_A)
	$(PFX)objdump -x $(NEWLIB_LIBC_A)
$(LDFLAGS_NEWLIB):
	make -C newlib


# libgcc needs to come last
CFLAGS += -O3 -fomit-frame-pointer -fdata-sections -ffunction-sections 
LDFLAGS += $(LIBGCC)

O := $(patsubst %.c,$(BUILD)/%.o,$(C))

build = @mkdir -p $(dir $(1)) ; echo [$(2)] $(notdir $(1)) ; $(3)

clean:
	rm -rf *~ build

$(BUILD)/%.o: %.s Makefile
	$(call build,$@,o,$(PFX)as -g $< -o $@)

$(BUILD)/%.o: %.c Makefile
	$(call build,$@,o,$(PFX)gcc $(CFLAGS) -g -c $< -o $@)

# For debug.
$(BUILD)/%.s: %.c Makefile
	$(call build,$@,s,$(PFX)gcc $(CFLAGS) -S -c $< -o $@)

$(BUILD)/%.a: $(O)
	$(call build,$@,a,$(PFX)ar -r $@ $(O))

$(BUILD)/%.elf:	$(BUILD)/%.o $(BUILD)/%.a %.ld
	$(call build,$@,elf,$(PFX)ld -Map $(BUILD)/$*.map -nostdlib -nostartfiles -T $*.ld $< $(O) -o $@ $(BUILD)/$*.a $(LDFLAGS))
