#ifndef _MEM_H_
#define _MEM_H_

#include <ctype.h>
#include <setjmp.h>

/* Basic memory model for the Scheme and PF scripting languages.  This
   includes GCd vectors, and opaque primitive leaf types and
   primitive functions. */

/* Lowelevel opaque primtive (leaf) objects. */
#include <leaf/symbol.h>
// #include <leaf/task.h>   // ck : abstraction for C stack
#include <leaf/port.h>
#include <leaf/bytes.h>
#include <ex/pair.h>
#include <leaf/prim.h>
#include <leaf/rc.h>
#include <leaf/inexact.h>
#include <leaf/channel.h>

/* Highlevel transparent object rep and GC */
#include <ex/object.h>
#include <ex/gc.h>

typedef struct {
    symbol_class *symbol_type;
    // ck_class *ck_type;
    prim_class *prim_type;
    port_class *port_type;
    bytes_class *bytes_type;
    rc_class *rc_type;
    inexact_class *inexact_type;
} base_types;

typedef struct _ex ex;
typedef object (*ex_m_write)(ex *ex, object ob);
typedef port*  (*_ex_m_port)(ex *ex);
typedef object (*_ex_m_leaf_to_object)(ex *ex, leaf_object*);
typedef leaf_object *(*_ex_m_object_to_leaf)(ex *ex, object);
typedef void (*_ex_m_object_erase_leaf)(ex *ex, object);
typedef object (*ex_m_make_pair)(ex *ex, object car, object cdr); // reader
struct _ex {
    void *type;  // in case VM structs are wrapped as LEAF objects
    void *ctx;   // any other user context associated with VM (i.e. JNIEnv)
    
    struct _gc *gc;   // garbage collected graph memory manager
    long entries;     // multiple entry semaphore
    jmp_buf except;   // GC unwind + exceptions
    
    prim *prim;            // current primitive
    long stateful_context; // if set, GC restarts are illegal
    _ error_tag;
    _ error_arg;

    /* VIRTUAL METHODS */

    /* Printing: delegate + current port. */
    ex_m_write write;
    _ex_m_port port;

    /* Parsing */
    ex_m_make_pair make_pair;

    /* Object wrapping */
    _ex_m_leaf_to_object leaf_to_object;
    _ex_m_object_to_leaf object_to_leaf;
    _ex_m_object_erase_leaf object_erase_leaf;
};

#define DEF_ATOM(name)                                         \
    static inline name *object_to_##name(object ob) {          \
        return (name*)object_struct(ob,name##_type()); }

// permanent constant objects
DEF_ATOM(prim)
DEF_ATOM(symbol)

typedef void* (*object_to_pointer)(object);

/* Printing is based on the assumption that GC_CONS is either a
   genuine constant (upper bits = 0) or points to an object which can
   be identified by its first field.  Primitive leaf types are
   recognized through the class list in mem. */

object _ex_write_vector(ex *ex, const char *type, vector *v);
object _ex_write(ex *ex, object ob);
_ _ex_printf(ex *pf, const char *fmt, ...);

/* Primitive definition table. */
typedef struct {
    const char *name;
    void *fn;
    int nargs;
} prim_def;

_ _is_vector_type(_ o, long flags);

#define TYPE_ERROR RAISE_TYPE_ERROR
#define ERROR(msg, o) ex_raise_error(EX, SYMBOL(msg), o)
#define INVALID(o) ERROR("invalid", o)
/* Pointer casts (just like predicates) are derived from the
   object_to_pointer function, _except_ for integers: there we use the
   predicate. */
// void* _ex_unwrap_pointer(ex *sc, void *unwrap, object o);
_ ex_raise_type_error(ex *ex, _ arg_o);
static inline void* _ex_unwrap_pointer(ex *ex, void *unwrap, object o){
    object_to_pointer fn = (object_to_pointer)unwrap;
    void *x = fn(o);
    if (unlikely(!x)) ex_raise_type_error(ex, o);
    return x;
}
long _ex_unwrap_integer(ex *ex, object o);
#define CAST(type,x) ((type*)(_ex_unwrap_pointer(EX, object_to_##type, x)))
#define CAST_INTEGER(x) _ex_unwrap_integer(EX, x)
#define CAST_CHAR(x)    _ex_unwrap_char(EX, x)

#define EXCEPT_TRY   0
#define EXCEPT_ABORT 1 /* abort to default toplevel continuation. */
#define EXCEPT_GC    2 /* garbage collection finished: restart primitive */

_ _ex_make_bytes(ex *ex, int size);
_ _ex_make_string(ex *ex, const char *str);
_ _ex_make_qstring(ex *ex, const char *str);
_ _ex_make_symbol(ex *ex, const char *str);
_ _ex_make_inexact(ex *ex, double d);
#define SYMBOL(str)   _ex_make_symbol(EX, str)
#define STRING(str)   _ex_make_string(EX, str)
#define INEXACT(d)    _ex_make_inexact(EX, d)
#define QSTRING(str)  _ex_make_qstring(EX, str) // with quotes
#define CHAR(str)     integer_to_object(str[0])
#define NAMED_CHAR(str) integer_to_object(named_char(str))

static inline int named_char (const char *name) {
    char x[strlen(name)+1];
    int i;
    if (!name[1]) return name[0]; // ordinary chars
    for(i=0; name[i]; i++) x[i] = tolower(name[i]);
    x[i] = 0;
    if (!strcmp(x, "space")) return ' ';
    if (!strcmp(x, "newline")) return '\n';
    return -1;
}

_ _ex_restart(ex *ex);

void _ex_overflow(ex *ex, long extra);

// parse improper list
_ ex_is_pair(ex *ex, _ o);
static inline void _ex_length_rest(ex *ex, _ lst, _ *length, _ *rest) {
    long nb = 0;
    while (object_to_lpair(lst) || 
           object_to_pair(lst)) { 
        nb++;
        lst = _CDR(lst); 
    }
    *rest = lst;
    *length = integer_to_object(nb);
}

/* EX primitives */
#define MAX_PRIM_ARGS 5
typedef _ (*ex_0)(ex* ex);
typedef _ (*ex_1)(ex* ex, _);
typedef _ (*ex_2)(ex* ex, _, _);
typedef _ (*ex_3)(ex* ex, _, _, _);
typedef _ (*ex_4)(ex* ex, _, _, _, _);
typedef _ (*ex_5)(ex* ex, _, _, _, _, _);

_ _ex_map1_prim(ex *ex, ex_1 fn, _ l_in);
_ _ex_map2_prim(ex *ex, ex_2 fn, _ l_in1, _ l_in2);

#define STRUCT(flags, size, ...) \
    return gc_make_tagged(EX->gc, flags, size, __VA_ARGS__)

#define ZERO integer_to_object(0)
#define ONE  integer_to_object(1)

_ _ex_read(ex *ex, port *input_port);
_ _ex_boot_load(ex *ex,  const char *bootfile);


#define VEC(x) (vector_to_object(((void*)(x))))

#define DECL_TYPE(name) \
    name *object_to_##name(object ob);

DECL_TYPE(port)
DECL_TYPE(channel)
DECL_TYPE(inexact)
DECL_TYPE(bytes)
typedef char cstring;  // for CAST()
DECL_TYPE(cstring)

// This is not a function, and not defined as an API symbol.

static inline _ _ex_leaf_to_object(ex *ex, void *leaf) {
    return ex->leaf_to_object(ex, (leaf_object*)leaf);
}





/* The garbage collector needs to restart the C stack after it
   performs a collection.  This is because it moves around data,
   making any references on the C stack invalid.  By default, in
   primitives, it is made illegal to perform restarts to avoid bugs
   in primitive code with side effects.

   This works as long as the primitives have bounded memory
   allocation.  However, for some, allocation is not bounded.  These
   primitives might need to be restarted.  Permission needs to be
   granted explicitly using the following macro.  In practice this
   means the code performs no (noticable) side effects that would
   prevent pre-emption and restart.  (I.e. it is purely functional).

   This appears in some functions in ex.c that create lists.

*/
#define ENABLE_RESTART() EX->stateful_context=0


_ _ex_make_bytes_port(ex *ex, bytes *b);
_ _ex_make_file_port(ex *ex, FILE *f, const char *name);


double object_to_double(_ ob);

#endif

