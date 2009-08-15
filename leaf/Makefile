
SRC=task.c port.c bytes.c symbol.c 


OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: leaf.a

leaf.a: $(OBJ)
	ar rcs leaf.a $(OBJ)

# Order is important!  ../gc contains default config which we override.
CPPFLAGS += -I../scheme/ -I../gc/ -I../task/ -DTASK_DEBUG=0


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
	rm -f *.o *.d *.d.* *~ *.a


