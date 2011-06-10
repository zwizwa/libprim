# Define objects in this module
MODULE_OBJ := main.o

# Define a new local build target.
TARGET         := main

# Make globally accessible target.
.PHONY: ecos_etst
ecos_test: $(BUILD)/main


