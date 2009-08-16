
SRC=ex.c gc.c ex_prims.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

%.h_ex_prims: %.c prims.ss
	pwd
	mzscheme prims.ss $< >$@

GEN = ex_prims.h_ex_prims

all: $(GEN) ex.a

ex.a: $(OBJ)
	ar rcs ex.a $(OBJ)


CPPFLAGS += -I../leaf


COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) 

# make deps from .c file
%.d: %.c Makefile $(GEN)
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)

%.o: %.c Makefile $(GEN)
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.d *.d.* *~ *.h_* *.a
