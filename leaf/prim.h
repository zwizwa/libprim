#ifndef _PRIM_H_
#define _PRIM_H_

#include <ex/object.h>

/* primitive function wrapper */
typedef struct {
    // void *free;
} prim_class;
typedef struct {
    leaf_object base; // FIXME: implement leaf object api
    void *fn;
    long nargs;
    /* Note: in general it is not allowed to place objects in atom
       structs, but in this case it's a symbol, so will never
       change. */
    object var;
} prim;

/* FIXME: turn this into a proper class object. */
static inline leaf_class* prim_type(void) {
    return (leaf_class*)0xF001; // dummy class
}

static inline long prim_nargs(prim *p){ return p->nargs; }
static inline void *prim_fn(prim *p)  { return p->fn; }



#endif
