
# 'MODULE_OBJ' contains this module's binary objects that will be
# collected in a $(MODULE).a archive.

MODULE_OBJ := \
	port.o bytes.o symbol.o scanner.o parser.o tuple.o leaf.o grid.o \
	inexact.o channel.o console.o bytecode.o

# TARGET         := libleaf.so
# TARGET_LDFLAGS := -shared -fPIC
