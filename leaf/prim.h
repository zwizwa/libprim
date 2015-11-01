#ifndef _PRIM_H_
#define _PRIM_H_

#include <ex/object.h>

/* primitive function wrapper */
typedef struct {
    leaf_class c;
} prim_class;
typedef struct {
    LEAF_OBJECT(base); // FIXME: implement leaf object api
    void *fn;
    long nargs;
    /* Note: in general it is not allowed to place objects in atom
       structs, but in this case it's a symbol, so will never
       change. */
    object var;
} prim;


static inline long prim_nargs(prim *p){ return p->nargs; }
static inline void *prim_fn(prim *p)  { return p->fn; }

leaf_class* prim_type(void);


#endif
