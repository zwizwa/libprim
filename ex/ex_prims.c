
/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) are based on a shared expression language which handles GC
   allocation and primitive exceptions. */


#include "ex.h"
#include "ex_prims.h_ex_prims"

#define EX ex

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
_ ex_is_integer(ex *ex, _ o) {
    if (GC_INTEGER == GC_TAG(o)) return TRUE;
    return FALSE;
}


_ ex_is_pair(ex *ex, _ o)  { return _is_vector_type(o, TAG_PAIR); }




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

_ ex_car(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->car; }
_ ex_cdr(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->cdr; }
_ ex_cadr(ex *ex, _ o) { pair *p = CAST(pair, ex_cdr(ex, o)); return p->car; }





_ ex_find_slot(ex *ex, _ E, _ var) {
    if (TRUE == ex_is_null(ex, E)) return FALSE;
    _ slot = CAR(E);
    _ name = CAR(slot);
    if (name == var) return slot;
    else return ex_find_slot(ex, CDR(E), var);
}
_ ex_find(ex *ex, _ E, _ var) {
    _ rv = ex_find_slot(ex, E, var);
    if (FALSE == IS_PAIR(rv)) return FALSE;
    return CDR(rv);
}


/* ERRORS */

_ ex_raise_error(ex *ex, _ sym_o, _ arg_o) {
    ex->error_tag = sym_o;
    ex->error_arg = arg_o;
    // if (sym_o != SYMBOL("halt")) ex_trap(ex);
    if (ex->entries) longjmp(ex->r.step, EXCEPT_ABORT);
    _ex_printf(ex, "ERROR: attempt to abort primitive outside of the main loop.\n");
    TRAP();
    exit(1);
}

_ ex_raise_type_error(ex *ex, _ arg_o) {
    return ex_raise_error(ex, SYMBOL("type"), arg_o);
}
