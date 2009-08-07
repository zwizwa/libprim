
SRC=gc.c symbol.c scheme.c main.c
OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

all: scheme

test.o: test.c_
scheme.o: boot.c_ scheme.h_


GEN = scheme.h_ test.c_ boot.c_

# make deps from .c file
%.d: %.c $(GEN) Makefile
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)

%.o: %.c Makefile $(GEN)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<

%.c_: %.scm unparse.ss
	mzscheme unparse.ss <$< >$@	
%.h_: %.c prims.ss
	mzscheme prims.ss <$< >$@



scheme: $(OBJ)
	$(CC) -o scheme $(OBJ)

strip: scheme
	strip scheme
	ls -l scheme



.PHONY: clean
clean:
	rm -f scheme *.o *.d *.d.* *~

.PHONY: mrproper
mrproper: clean
	rm -f *.c_ *.h_	