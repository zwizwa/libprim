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

/* Ref management wrapper. */
typedef struct {
} rc_class;
typedef struct {
    rc_class *type;
    void *object;
    int rc;
    fin free;
} rc;


struct _gc;

typedef object _;  // Highly effective noise reduction.

typedef struct {
    void *type;

    /* Garbage collector. */
    struct _gc *gc;

    /* Primitive datatypes. */
    symbol_class *symbol_type;
    ck_class *ck_type;
    prim_class *prim_type;
    port_class *port_type;
    bytes_class *bytes_type;
    rc_class *rc_type;
    
} mem;

#define DEF_CONST_TYPE(name)                                           \
    static inline name *object_to_##name(object ob, mem *m) {          \
        return (name*)object_struct(ob,m->name##_type); }

// permanent constant objects
DEF_CONST_TYPE(prim)
DEF_CONST_TYPE(symbol)

typedef void* (*object_to_pointer)(object, mem*);

/* Printing is based on the assumption that GC_CONST is either a
   genuine constant (upper bits = 0) or points to an object which can
   be identified by its first field.  Primitive leaf types are
   recognized through the class list in mem. */

typedef object (*object_write_delegate)(void *ctx, object ob);
object object_write_vector(const char *type, vector *v, port *p, mem *m,
                           object_write_delegate fn, void *ctx);
object object_write(object ob, port *p, mem *m,
                    object_write_delegate fn, void *ctx);



#endif
