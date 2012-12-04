NEWLIB := newlib-1.18.0


NEWLIB_TAR := $(NEWLIB).tar.gz
NEWLIB_URL := ftp://sources.redhat.com/pub/newlib/$(NEWLIB_TAR)
NEWLIB_STAMP := $(NEWLIB)/.stamp
NEWLIB_MAKEFILE := $(NEWLIB)/Makefile
NEWLIB_A := $(NEWLIB)/libc.a

all: $(NEWLIB_A)

clean:
	rm -rf $(NEWLIB)
mrproper: clean
	rm -f $(NEWLIB_TAR)

# Download
$(NEWLIB_TAR): $(NEWLIB_URL)
	wget --passive-ftp $<
# Unpack
$(NEWLIB_STAMP): $(NEWLIB_TAR)
	tar xf $<
	touch $@
# Configure
$(NEWLIB_MAKEFILE): $(NEWLIB_STAMP)
	cd $(NEWLIB) ; ./configure --target arm-eabi --disable-newlib-supplied-syscalls
# Build
$(NEWLIB_A): $(NEWLIB_MAKEFILE)
	make -C $(NEWLIB)
