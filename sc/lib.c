#include "scheme.h"
#include "../ex/ex_prims.h_ex_prims"
#include "scheme.h_sc_prims"

#undef EX
#define EX ex
#include "boot.h_load"


void _sc_load_lib(ex *ex) {
    EVAL(CONS(SYMBOL("bool?"),CONS(NUMBER(123),NIL)));
    _load(ex);
}
