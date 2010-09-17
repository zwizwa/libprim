# -*- makefile -*-

# A makefile include for building libprim on eCos.
# This requires the variable $(LIBPRIM) to be defined.

LIBPRIM_CFLAGS = -I$(LIBPRIM)/build -I$(LIBPRIM) 
LIBPRIM_SOURCES  := \
	$(LIBPRIM)/leaf/channel.c \
	$(LIBPRIM)/leaf/port.c \
	$(LIBPRIM)/leaf/bytes.c  \
	$(LIBPRIM)/leaf/parser.c \
	$(LIBPRIM)/leaf/scanner.c \
	$(LIBPRIM)/leaf/tuple.c \
	$(LIBPRIM)/leaf/leaf.c \
	$(LIBPRIM)/leaf/symbol.c \
	$(LIBPRIM)/leaf/inexact.c \
	$(LIBPRIM)/leaf/console.c \
	$(LIBPRIM)/ex/ex.c  \
	$(LIBPRIM)/ex/read_intl.c  \
	$(LIBPRIM)/ex/gc.c \
	$(LIBPRIM)/sc/sc.c \
	$(LIBPRIM)/sc_vm1/vm1.c


