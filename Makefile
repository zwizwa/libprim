
DELEGATE = all install clean sc_vm1_test sc_vm2_test sc_vm3_test
.PHONY: $(DELEGATE) mrproper

# Generate delegate rules
define tobuild
$(1):
	@$$(MAKE) -sC build $(1)
endef
$(foreach target,$(DELEGATE),$(eval $(call tobuild,$(target))))

# Clean build + config.
mrproper:
	rm -rf build
# Error: this Makefile needs build/ dir.
build:
	@echo Run ./configure first ; false
