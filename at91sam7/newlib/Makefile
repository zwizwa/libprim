NEWLIB := newlib-1.20.0

NEWLIB_TAR := $(NEWLIB).tar.gz
NEWLIB_URL := ftp://sources.redhat.com/pub/newlib/$(NEWLIB_TAR)
NEWLIB_A := $(NEWLIB)/libc.a

# Build tools need to be in path.
PATH := /opt/xc/gnutools/bin:$(shell echo $$PATH)
export PATH

all: $(NEWLIB_A)

clean:
	rm -rf $(NEWLIB)
mrproper: clean
	rm -f $(NEWLIB_TAR)

# Download
$(NEWLIB_TAR):
	wget --passive-ftp $(NEWLIB_URL)

# Unpack
$(NEWLIB)/.unpacked: $(NEWLIB_TAR)
	tar xf $<
	touch $@
# Configure
$(NEWLIB)/Makefile: $(NEWLIB)/.unpacked
	cd $(NEWLIB) ; ./configure --target arm-none-eabi --disable-newlib-supplied-syscalls
# Build
$(NEWLIB)/libc.a: $(NEWLIB)/Makefile
	make -C $(NEWLIB)
