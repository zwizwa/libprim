ifeq ($(ARCH),ecos)

m_C       := main.c
m_TARGET  := main

# Make globally accessible target.
.PHONY: ecos_test
ecos_test: $(m_BUILD)/main

endif

