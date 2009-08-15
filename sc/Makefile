
SRC=lib.c scheme.c main.c 
OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: scheme $(DEP_OBJS)

# generated files
%.h_scm: %.scm ../unparse.ss
	mzscheme ../unparse.ss <$< >$@
%.h_sc_prims: %.c prims.ss
	mzscheme prims.ss $< >$@

GEN = test.h_scm scheme.h_sc_prims boot.h_scm

# Order is important!  ../gc contains default config which we override.
CPPFLAGS += -I../scheme/ -I../gc/ -I../leaf/ -I../ex/ -DTASK_DEBUG=0


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


DEP_OBJ=../leaf/leaf.a ../ex/ex.a

scheme: $(OBJ) $(DEP_OBJ)
	$(CC) -o scheme $(OBJ) $(DEP_OBJ)

.PHONY: clean
clean:
	rm -f scheme *.o *.d *.d.* *~ *.h_

