
/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) are based on a shared expression language which handles GC
   allocation and primitive exceptions. */


#include "ex.h"
#include "ex_prims.h_"

/* Booleans are GC_CONST */
_ ex_is_bool(ex *ex, _ o) {
    void *x;
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}


/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
_ ex_trap(ex *ex) {
    kill(getpid(), SIGTRAP);
    return VOID;
}


_ ex_cons(ex *ex, _ car, _ cdr) {
    vector *v = gc_alloc(ex->gc, 2);
    vector_set_flags(v, TAG_PAIR);
    v->slot[0] = car;
    v->slot[1] = cdr;
    return vector_to_object(v);
}
