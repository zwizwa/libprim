
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
/* The empty list is the NULL pointer */
_ ex_is_null(ex *ex, _ o) {
    if (NIL == o) return TRUE; else return FALSE;
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





_ ex_find_slot(ex *ex, _ E, _ var) {
    if (TRUE == ex_is_null(ex, E)) return FALSE;
    _ slot = CAR(E);
    _ name = CAR(slot);
    if (name == var) return slot;
    else return ex_find_slot(ex, CDR(E), var);
}
_ ex_find(ex *ex, _ E, _ var) {
    _ rv = ex_find_slot(ex, E, var);
    if (FALSE == ex_is_pair(ex, rv)) return FALSE;
    return CDR(rv);
}
