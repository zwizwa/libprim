
# 'MODULE_OBJ' contains this module's binary objects that will be
# collected in a $(MODULE).a archive.

MODULE_OBJ := \
	port_posix.o channel.o

TARGET         := libleaf.so
TARGET_LDFLAGS := -shared -fPIC
