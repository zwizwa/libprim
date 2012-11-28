
SRC=ffmpeg.c xwindow.c xv.c glx.c gsl.c yuv.c sc_wrap.c
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
CPPFLAGS += `gsl-config --cflags`

# Hack to make it work if libprim is not installed.
CPPFLAGS += -I..

include ../Makefile.rules


# Note that this only contains leaf objects.  The wrappers still need
# to be generated + the eventual application or shared lib needs to be
# linked to the external libraries.  Currently this code is not
# available.

# media?
ifdef __MEDIA
DEP_OBJ+=../media/libprim_media.a
SRC += media.c
CPPFLAGS += -DHAVE_MEDIA
# FFMPEG
LIBS+=`pkg-config libavformat --libs`
CPPFLAGS += `pkg-config libavformat --cflags`
# X11
LIBS += -L/usr/X11R6/lib -lX11 -lXext -lXv -lGL -lGLU
CPPFLAGS += -I/usr/X11R6/include
# GSL
LIBS+=`gsl-config --libs`
endif


