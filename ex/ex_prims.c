
/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) are based on a shared expression language which handles GC
   allocation and primitive exceptions. */


#include "ex.h"

/* Booleans are GC_CONST */
_ ex_is_bool(ex *ex, _ o) {
    void *x;
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}

