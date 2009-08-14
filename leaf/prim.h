#ifndef _PRIM_H_
#define _PRIM_H_

/* primitive function wrapper */
typedef struct {
    // void *free;
} prim_class;
typedef struct {
    void *type;
    void *fn;
    long nargs;
    /* Note: in general it is not allowed to place objects in atom
       structs, but in this case it's a symbol, so will never
       change. */
    object var;
} prim;


#endif
