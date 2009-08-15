
SRC=ex.c gc.c

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: $(OBJ)

CPPFLAGS += -I../leaf


COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) 

# make deps from .c file
%.d: %.c Makefile
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)

%.o: %.c Makefile
	$(COMPILE) -o $@ -c $<

.PHONY: clean
clean:
	rm -f *.o *.d *.d.* *~

