#ifndef _MEM_H_
#define _MEM_H_

/* Loweleve opaque primtive (leaf) objects. */
#include "symbol.h"
#include "task.h"
#include "port.h"
#include "bytes.h"
#include "pair.h"
#include "prim.h"

/* Highlevel transparent object rep and GC */
#include "object.h"
#include "gc.h"


typedef object _;  // Highly effective noise reduction.

typedef struct {
    void *type;

    /* Delegate objects. */
    gc *gc;

    /* Primitive datatypes. */
    symbol_class *symbol_type;
    ck_class *ck_type;
    prim_class *prim_type;
    port_class *port_type;
    bytes_class *bytes_type;

    
} mem;

#define DEF_CONST_TYPE(name)                                           \
    static inline name *object_to_##name(object ob, mem *m) {          \
        return (name*)object_struct(ob,m->name##_type); }


typedef void* (*object_to_pointer)(object, mem*);


#endif
