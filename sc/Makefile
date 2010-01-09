MODULE=sc
APP=sc
SRC=scheme.c main.c
# lib.c
DEP_OBJ=$(BUILDDIR)/ex/libprim_ex.a $(BUILDDIR)/leaf/libprim_leaf.a 


LIBS += -lpthread

# Note: libprim_media.a isn't linked in.  These objects are currently
# used in a different project, with special-purpose wrappers.  For
# libprim SC/PF this will eventually be replaced with automatic
# wrappers.

# # media?
# ifdef MEDIA
# DEP_OBJ+=../media/libprim_media.a
# SRC += media.c
# CPPFLAGS += -DHAVE_MEDIA
# # FFMPEG
# LIBS+=`pkg-config libavformat --libs`
# CPPFLAGS += `pkg-config libavformat --cflags`
# # X11
# LIBS += -L/usr/X11R6/lib -lX11 -lXext -lXv -lGL -lGLU
# CPPFLAGS += -I/usr/X11R6/include
# # GSL
# LIBS+=`gsl-config --libs`
# endif

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

SC_LIB_OBJ = scheme.o
SC_APP_OBJ = $(SC_LIB_OBJ) main.o $(DEP_OBJ)


all: $(APP) libprim_sc.a
#libprim_sc_media.a

libprim_sc.a: $(SC_LIB_OBJ)
	ar rcs libprim_sc.a $(SC_LIB_OBJ)


# generated files
# %.h_load: %.scm ../load.ss
#	$(MZSCHEME) ../load.ss $< >$@

GEN = scheme.h_prims 
# test.h_load
# boot.h_load 

include ../Makefile.defs
include ../Makefile.rules

# There are 3 stages in compiling the boot script:
#  1. defines 'eval' and 'expand' on top of SC VM
#  2. defines library code using the expander
#  3. expands the whole boot to run directly on SC VM

# code in step 2 is reused elsewhere.  step 1 is specific to this VM.

BOOT=   boot12.gscm

MRPROPER += $(BOOT) $(BOOTE)

$(BOOT): boot1.scm boot2.scm
	$(MZSCHEME) ./bootstrap.ss >$(BOOT)

test: $(BOOT) $(APP)
	gdb --args ./sc --boot $(BOOT)

tcptest: $(BOOTE)
	./sc --boot $(BOOTE) --eval '(tcp-console-server 12345)'
