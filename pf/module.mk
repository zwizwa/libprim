LOCAL_OBJ := pf.o px.o

# Application target: 
# Define a new global build target.
LOCAL_APP         := pf
LOCAL_APP_MODULES := ex leaf
LOCAL_APP_LDFLAGS := -lpthread

