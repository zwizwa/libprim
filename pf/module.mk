LOCAL_OBJ := pf.o px.o

# Application target: 
# Define a new global build target.
TARGET         := pf
TARGET_MODULES := ex leaf
TARGET_LDFLAGS := -lpthread

