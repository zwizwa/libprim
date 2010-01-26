
DELEGATE = all install clean sc_vm1_test sc_vm2_test sc_vm3_test
.PHONY: $(DELEGATE) mrproper

# Generate delegate rules
define tobuild
$(1): build
	@$$(MAKE) -sC build $(1)
endef
$(foreach target,$(DELEGATE),$(eval $(call tobuild,$(target))))

# Clean build + config.
mrproper:
	rm -rf build

# Run ./configure automatically if there's no build dir.
build:
	./configure

release: 
	rm -rf build && ./configure && make -j4

debug: 
	rm -rf build && ./configure --enable-debug && make -j4
