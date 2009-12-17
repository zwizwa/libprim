include ../Makefile.defs

all: tests

MODULE=test

%.test: $(SRCDIR)/$(MODULE)/%.c
	@$(CC) $(CPPFLAGS) $(CFLAGS) $(OPTI_CFLAGS) $(DEBUG_CFLAGS) $< -o $@ $(LIBS)
	@ rm -f $@

tests: $(TESTS)

clean:
	rm -f log *.test *~


