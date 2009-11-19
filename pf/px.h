#ifndef _PF_PX_H_
#define _PF_PX_H_

#include "pf.h_pf_prims"
#include "px.h_px_prims"

static void _exch(_*a, _*b) {
    _ tmp = *a;
    *a = *b;
    *b = tmp;
}
_ static inline _move(_ *ob, _ filler) {
    _ o = *ob; 
    *ob = filler; 
    return o; 
}
#define EXCH(a,b) _exch(&a, &b)
#define MOVE(from,filler) _move(&from,filler)
#define FROM_TO(a,b) _px_from_to(pf, &(pf->a), &(pf->b))
#define TOP    _px_top(pf)
#define SECOND _px_second(pf)
#define _TOP _CAR(pf->p)  /* UNSAFE */

void _px_unlink(pf* pf, _ ob);
_ _px_link(pf *pf, _ ob);
_ _px_top(pf *pf);
_ _px_second(pf *pf);

// don't use this to manipulate the freelist directly.
static inline void _px_from_to(pf *pf, _ *from, _ *to) {
    if (unlikely(NIL == *from)) px_error_underflow(pf);
    _ pair =  *from;
    *from = _CDR(*from);
    _CDR(pair) = *to;
    *to = pair;
}
static inline void _px_to_free(pf *pf, _ *from) {
    _px_from_to(pf, from, &pf->freelist);
    vector_reset_flags(object_to_vector(pf->freelist), TAG_LPAIR);
}
static inline void _px_drop(pf *pf, _ *stack) {
    _px_to_free(pf, stack);  // this catches underflow errors
    _ ob = MOVE(_CAR(pf->freelist), VOID);
    _px_unlink(pf, ob);
}
static inline void _px_push(pf *pf, _ ob) {
    pf->p = LINEAR_CONS(ob, pf->p);
}

static inline _ _px_alloc_cells(pf *pf, long nb) {
    _ cells = NIL;
    // _ex_printf(EX, "alloc %d cells\n", (int)nb);
    while (nb--) {
        cells = LCONS(VOID, cells);
    }
    return cells;
}
static inline void _px_need_free(pf *pf) {
    if (unlikely(NIL == pf->freelist))  {
        pf->freelist = _px_alloc_cells(pf, 1);
    }
}


/* Constant types are embedded in GC_CONST pointers, and can be
   identified by their first field (void*). */
DEF_ATOM(rc)
static inline void *object_rc_struct(object ob, void *type) {
    rc *x = object_to_rc(ob);
    if (!x) return NULL;
    void *x_type = *((void**)x->ctx);
    if (x_type != type) return NULL;
    return x->ctx;
}

leaf_object *_px_object_to_leaf(pf *pf, _ ob);
object _px_make_rc(pf *pf, leaf_object *x);

/* RC types are const types wrapped in a RC struct. */
#define DEF_RC_TYPE(name)                                              \
    name *object_to_##name(object ob) {                                \
        return (name*)object_rc_struct(ob,name##_type()); }

port *_px_port(pf *pf);
_ _px_make_port(pf *pf, FILE *f, const char *name);
_ _px_make_string(pf *pf, const char *name);
_ _px_make_qstring(pf *pf, const char *name);

#endif
