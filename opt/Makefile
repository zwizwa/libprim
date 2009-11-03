
SRC=ffmpeg.c


OBJ=$(SRC:.c=.o)

DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: opt/a

opt.a: $(OBJ)
	ar rcs opt.a $(OBJ)

include ../Makefile.rules

