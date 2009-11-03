
SRC=ffmpeg.c


OBJ=$(SRC:.c=.o)

DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32


libprim_media.a: $(OBJ)
	ar rcs libprim_media.a $(OBJ)

CPPFLAGS += `pkg-config libavformat --cflags`

include ../Makefile.rules

