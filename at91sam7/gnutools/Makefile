TARGET := arm-none-eabi
PREFIX := /opt/xc/gnutools

GCC_VERSION      := 4.7.2
BINUTILS_VERSION := 2.23
GDB_VERSION      := 7.5.1
NEWLIB_VERSION   := 1.20.0

GNU_REPO    := ftp://ftp.gnu.org/gnu
NEWLIB_REPO := ftp://sources.redhat.com/pub/newlib


# -------------------------------------------------------------
GCC      := gcc-$(GCC_VERSION)
BINUTILS := binutils-$(BINUTILS_VERSION)
GDB      := gdb-$(GDB_VERSION)
NEWLIB   := newlib-$(NEWLIB_VERSION)

COMPONENTS := $(BINUTILS)
ALL := \
	build/$(BINUTILS)/.build \
	build/$(GCC)/.build \
	build/$(NEWLIB)/.build \
	build/$(GCC)/.build_pass2 \
	build/$(GDB)/.build

all: $(ALL)

URLS := \
	$(GNU_REPO)/gcc/$(GCC)/$(GCC).tar.gz \
	$(GNU_REPO)/binutils/$(BINUTILS).tar.gz \
	$(GNU_REPO)/gdb/$(GDB).tar.gz \
	$(NEWLIB_REPO)/$(NEWLIB).tar.gz \

include ../../download.mk


PATH := $(PREFIX)/bin:$(shell echo $$PATH)
export PATH

CONFIGURE_OPTS := \
	--target=$(TARGET) \
	--prefix=$(PREFIX) \
	--enable-interwork \
	--enable-multilib

CONFIGURE_OPS_GCC := \
	$(CONFIGURE_OPTS) \
	--enable-languages="c,c++" \
	--with-system-zlib \
	--with-newlib \
	--with-headers=../../$(NEWLIB)/newlib/libc/include 

# The tools build in the order shown below through .stamp dependencies.

build/$(BINUTILS)/.build: $(BINUTILS)/.unpack
	mkdir -p $(dir $@)
	cd $(dir $@) && ../../$(BINUTILS)/configure $(CONFIGURE_OPTS)
	+make -C $(dir $@) all
	+make -C $(dir $@) install
	touch $@

build/$(GCC)/.build: build/$(BINUTILS)/.build $(GCC)/.unpack $(NEWLIB)/.unpack
	mkdir -p $(dir $@)
	cd $(dir $@) && ../../$(GCC)/configure $(CONFIGURE_OPTS_GCC)
	+make -C $(dir $@) all-gcc
	+make -C $(dir $@) install-gcc
	touch $@

build/$(NEWLIB)/.build: build/$(GCC)/.build
	mkdir -p $(dir $@)
	cd $(dir $@) && ../../$(NEWLIB)/configure $(CONFIGURE_OPTS)
	env | grep PATH
	+make -C $(dir $@) all
	+make -C $(dir $@) install
	touch $@

build/$(GCC)/.build_pass2: build/$(NEWLIB)/.build
	+make -C $(dir $@) all
	+make -C $(dir $@) install
	touch $@

# Does this really depend on target GCC?
build/$(GDB)/.build: $(GDB)/.unpack build/$(GCC)/.build_pass2
	mkdir -p $(dir $@)
	cd $(dir $@) && ../../$(GDB)/configure $(CONFIGURE_OPTS)
	+make -C $(dir $@) all
	+make -C $(dir $@) install
	touch $@



