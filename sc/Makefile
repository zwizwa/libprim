
SRC=lib.c scheme.c main.c
OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: scheme

# generated files
%.h_: %.scm ../unparse.ss
	mzscheme ../unparse.ss <$< >$@
%.h_: %.c prims.ss
	mzscheme prims.ss <$< >$@

GEN = test.h_ scheme.h_ boot.h_

# Order is important!  ../gc contains default config which we override.
CPPFLAGS += -I../scheme/ -I../gc/ -I../leaf/ -I../object/ -DTASK_DEBUG=0


COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) 

# make deps from .c file
%.d: %.c Makefile $(GEN)
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)

%.o: %.c Makefile
	$(COMPILE) -o $@ -c $<


.PHONY: deps
deps:
	make -C ../leaf
	make -C ../object

DEP_OBJS=../leaf/*.o ../object/*.o

scheme: $(OBJ) deps
	$(CC) -o scheme $(OBJ) $(DEP_OBJS)

.PHONY: clean
clean:
	rm -f scheme *.o *.d *.d.* *~

.PHONY: mrproper
mrproper: clean
	make -C ../leaf clean
	rm -f *.c_ *.h_	