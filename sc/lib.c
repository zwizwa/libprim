#include "scheme.h"
#include "../ex/ex_prims.h_ex_prims"
#include "scheme.h_sc_prims"
#include "boot.h_scm"
void _sc_load_lib(sc *sc) {
    _load(sc);
}
