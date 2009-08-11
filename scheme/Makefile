
SRC=gc.c task.c symbol.c lib.c scheme.c main.c
OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g
# OPTI_CFLAGS=-Os

# CC = gcc -m32

all: scheme


# generated files
%.h_: %.scm unparse.ss
	mzscheme unparse.ss <$< >$@
%.h_: %.c prims.ss
	mzscheme prims.ss <$< >$@

GEN = test.h_ scheme.h_ boot.h_

# Order is important!  ../gc contains default config which we override.
CPPFLAGS += -I../scheme/ -I../gc/ -I../task/ -DTASK_DEBUG=0


COMPILE = $(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) 


gc.o: ../gc/gc.c
	$(COMPILE) -o $@ -c $<
task.o: ../task/task.c
	$(COMPILE) -o $@ -c $<

# make deps from .c file
%.d: %.c Makefile $(GEN)
	set -e; rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

-include $(SRC:.c=.d)

%.o: %.c Makefile
	$(COMPILE) -o $@ -c $<


scheme: $(OBJ)
	$(CC) -o scheme $(OBJ)

strip: 
	make clean
	make scheme
	strip *.o
	ls -l *.o


.PHONY: clean
clean:
	rm -f scheme *.o *.d *.d.* *~

.PHONY: mrproper
mrproper: clean
	rm -f *.c_ *.h_	