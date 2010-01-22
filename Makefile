
DELEGATE = all install clean vm_test sc_test
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
