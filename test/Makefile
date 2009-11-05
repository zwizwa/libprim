include ../Makefile.defs

all: tests

%.test: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) $< -o $@ $(LIBS)
	@ rm -f $@

tests: $(TESTS)

clean:
	rm -f log *.test *~
