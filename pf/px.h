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
#define TOP  _px_top(pf)
#define _TOP _CAR(pf->ds)  /* UNSAFE */

void _px_need_free(pf *pf);
void _px_unlink(pf* pf, _ ob);
_ _px_link(pf *pf, _ ob);
_ _px_top(pf *pf);

static inline void _px_from_to(pf *pf, _ *from, _ *to) {
    if (unlikely(NIL == *from)) px_error_underflow(pf);
    _ pair =  *from;
    *from = _CDR(*from);
    _CDR(pair) = *to;
    *to = pair;
}
static inline void _px_drop(pf *pf, _ *stack) {
    _px_from_to(pf, stack, &pf->free);  // this catches underflow errors
    _ ob = MOVE(_CAR(pf->free), VOID);
    _px_unlink(pf, ob);
}
static inline void _px_push(pf *pf, _ ob) {
    pf->ds = LINEAR_CONS(ob, pf->ds);
}


/* Constant types are embedded in GC_CONST pointers, and can be
   identified by their first field (void*). */
DEF_ATOM(rc)
static inline void *object_rc_struct(object ob, ex *m, void *type) {
    rc *x = object_to_rc(ob, m);
    if (!x) return NULL;
    void *x_type = *((void**)x->ctx);
    if (x_type != type) return NULL;
    return x->ctx;
}

/* RC types are const types wrapped in a RC struct. */
#define DEF_RC_TYPE(name)                                              \
    static inline name *object_to_##name(object ob, ex *m) {          \
        return (name*)object_rc_struct(ob,m,m->p->name##_type); }

DEF_RC_TYPE(port)

port *_px_port(pf *pf);
_ _px_make_port(pf *pf, FILE *f, const char *name);

#endif