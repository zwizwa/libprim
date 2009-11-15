
SRC=ffmpeg.c xwindow.c xv.c glx.c
CPPFLAGS += -DPRIVATE

OBJ=$(SRC:.c=.o)

DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_media_prims: %.c media_prims.ss ../prim-tools.ss
	$(MZSCHEME) media_prims.ss $< >$@

libprim_media.a: $(OBJ)
	ar rcs libprim_media.a $(OBJ)

CPPFLAGS += `pkg-config libavformat --cflags`

include ../Makefile.rules

