LOCAL_OBJ := sc.o

# Define a new build target.
TARGET         := libsc.so
TARGET_MODULES := ex leaf
TARGET_LDFLAGS := -lpthread -shared -fPIC


