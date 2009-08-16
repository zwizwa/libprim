#include "pf.h"


#undef EX
#define EX ex
#include "boot.h_load"

void _pf_load_lib(pf *pf) {
    _load((ex*)pf);
}
