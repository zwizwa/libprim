
APP=scheme
SRC=lib.c scheme.c main.c 
DEP_OBJ=../leaf/leaf.a ../ex/ex.a

OBJ=$(SRC:.c=.o)
DEBUG_CFLAGS=-Wall -g -Wno-unused
# OPTI_CFLAGS=-Os
# CC = gcc -m32

all: scheme $(DEP_OBJ)

# generated files
%.h_load: %.scm ../load.ss
	mzscheme ../load.ss $< >$@
%.h_sc_prims: %.c prims_sc.ss
	mzscheme prims_sc.ss $< >$@

GEN = scheme.h_sc_prims boot.h_load test.h_load

CPPFLAGS += -I../leaf/ -I../ex/ -DTASK_DEBUG=0
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

$(APP): $(OBJ) $(DEP_OBJ)
	$(CC) -o $(APP) $(OBJ) $(DEP_OBJ)

.PHONY: clean
clean:
	rm -f scheme *.o *.d *.d.* *~ *.h_*

