LOCAL_OBJ := pf.o px.o

# Application target: 
$(call app_rule,pf, ex leaf, -lpthread)
