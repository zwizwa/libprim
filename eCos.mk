# -*- makefile -*-

# A makefile include for building libprim on eCos / eCosPro.
# This requires the variable $(LIBPRIM) and $(LIBPRIM_BUILD) to be defined.

LIBPRIM_ECOS_CFLAGS = -I$(LIBPRIM_BUILD) -I$(LIBPRIM) 

LIBPRIM_ECOS_LIBS = \
	$(LIBPRIM_BUILD)/sc_vm1/sc_vm1.a \
	$(LIBPRIM_BUILD)/ex/ex.a \
	$(LIBPRIM_BUILD)/sc/sc.a \
	$(LIBPRIM_BUILD)/leaf/leaf.a \

### OLD

# LIBPRIM_ECOS_CFLAGS = -I$(LIBPRIM)/build -I$(LIBPRIM) 
# LIBPRIM_ECOS_SOURCES  := \
# 	$(LIBPRIM)/leaf/port.c \
# 	$(LIBPRIM)/leaf/bytes.c  \
# 	$(LIBPRIM)/leaf/parser.c \
# 	$(LIBPRIM)/leaf/scanner.c \
# 	$(LIBPRIM)/leaf/tuple.c \
# 	$(LIBPRIM)/leaf/leaf.c \
# 	$(LIBPRIM)/leaf/symbol.c \
# 	$(LIBPRIM)/leaf/inexact.c \
# 	$(LIBPRIM)/leaf/console.c \
# 	$(LIBPRIM)/ex/ex.c  \
# 	$(LIBPRIM)/ex/read_intl.c  \
# 	$(LIBPRIM)/ex/gc.c \
# 	$(LIBPRIM)/sc/sc.c \
# 	$(LIBPRIM)/sc_vm1/vm1.c

# #	$(LIBPRIM)/leaf/channel.c


# LIBPRIM_ECOS_MAIN := \
# 	$(LIBPRIM)/ecos/main.c

# # This is built by the libprim configure step.
# LIBPRIM_ECOS_BOOT_O := \
# 	$(LIBPRIM)/build/sc_vm1/boot.o
