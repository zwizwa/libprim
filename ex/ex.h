#ifndef _MEM_H_
#define _MEM_H_

/* Basic memory model for the Scheme and PF scripting languages.  This
   includes GCd vectors, and opaque primitive leaf types and
   primitive functions. */

/* Lowelevel opaque primtive (leaf) objects. */
#include "symbol.h"
#include "task.h"
#include "port.h"
#include "bytes.h"
#include "pair.h"
#include "prim.h"
#include "rc.h"

/* Highlevel transparent object rep and GC */
#include "object.h"
#include "gc.h"

typedef struct {
    symbol_class *symbol_type;
    ck_class *ck_type;
    prim_class *prim_type;
    port_class *port_type;
    bytes_class *bytes_type;
    rc_class *rc_type;
} base_types;

// Re-entrant part of ex state.
typedef struct {
    jmp_buf step;  // current CEKS step abort
    prim *prim;
} ex_r;

typedef struct _ex ex;
typedef object (*ex_write_method)(ex *ex, object ob);
typedef port*  (*_ex_port_method)(ex *ex);
struct _ex {
    void *type;

    /* Garbage collector. */
    struct _gc *gc;
    jmp_buf top;  // GC unwind
    long top_entries; // guard semaphore
    
    /* Primitive exceptions. */
    ex_r r;

    /* An extensible list of primitive data types.  During bootstrap
       and in the C code it is assumed to consist of only the base
       types. */
    base_types *p;

    /* Printing: delegate + current port. */
    ex_write_method write;
    _ex_port_method  port;

};

#define DEF_ATOM(name)                                                \
    static inline name *object_to_##name(object ob, ex *m) {          \
        return (name*)object_struct(ob,m->p->name##_type); }

// permanent constant objects
DEF_ATOM(prim)
DEF_ATOM(symbol)

typedef void* (*object_to_pointer)(object, ex*);

/* Printing is based on the assumption that GC_CONST is either a
   genuine constant (upper bits = 0) or points to an object which can
   be identified by its first field.  Primitive leaf types are
   recognized through the class list in mem. */

object _ex_write_vector(ex *ex, const char *type, vector *v);
object ex_write(ex *ex, object ob);
_ _ex_printf(ex *pf, const char *fmt, ...);

/* Primitive definition table. */
typedef struct {
    const char *name;
    void *fn;
    int nargs;
} prim_def;

#endif