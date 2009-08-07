
SRC=gc.c symbol.c scheme.c main.c
OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Wall -Os

all: scheme


# make deps from .c file
%.d: %.c
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)



%.o: %.c Makefile
	$(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) -o $@ -c $<

%.c_: %.scm unparse.ss
	mzscheme unparse.ss <$< >$@	

test.o: test.c_
scheme.o: boot.c_

scheme: $(OBJ)
	$(CC) -o scheme $(OBJ)

clean:
	rm -rf scheme *.o *.d *~ *.cgen
