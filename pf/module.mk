LOCAL_OBJ := pf.o px.o

# Application target: 
$(call app_rule, $(LOCAL_MODULE)/pf, pf ex leaf, -lpthread)
