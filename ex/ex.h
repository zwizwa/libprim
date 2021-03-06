#ifndef _MEM_H_
#define _MEM_H_

#include "config.h"
#include <ctype.h>

/* Basic memory model for the Scheme and PF scripting languages.  This
   includes GCd vectors, and opaque primitive leaf types and
   primitive functions. */

/* Lowelevel opaque primtive (leaf) objects. */
#include <leaf/symbol.h>
// #include <leaf/task.h>   // ck : abstraction for C stack
#include <leaf/port.h>
#include <leaf/bytes.h>
#include <leaf/prim.h>
//#include <leaf/rc.h>
#include <leaf/inexact.h>
#include <leaf/error.h>
#include <leaf/task.h>
#include <ex/pair.h>
#include <ex/object.h>

/* Highlevel transparent object rep and GC */
#include <ex/object.h>
#include <ex/gc.h>

typedef struct _ex ex;
#include <ex/ex.g.h>



typedef object (*ex_m_write)(ex *ex, object ob);
typedef port*  (*_ex_m_port)(ex *ex);
typedef object (*_ex_m_leaf_to_object)(ex *ex, leaf_object*);
typedef leaf_object *(*_ex_m_object_to_leaf)(ex *ex, object);
typedef void (*_ex_m_object_erase_leaf)(ex *ex, object);
typedef object (*ex_m_make_pair)(ex *ex, object car, object cdr); // reader
typedef void (*_ex_m_set_error)(ex *ex, prim *prim, _ error_tag, _ error_arg);
typedef void* (*_ex_m_object_ref_struct)(ex *ex, object ob, void *type);

struct _ex {
    leaf_ctx l;          // The EX context extends the LEAF context
    struct _gc *gc;      // garbage collected graph memory manager
    int gc_guard_cells;  // guard buffer for primitives
    prim *prim;          // current primitive
    void *ctx;           // any other user context associated with VM (i.e. JNIEnv)

    mutex_t machine_lock; // unlock machine struct during select() call

    long stateful_context; // if set, GC restarts are illegal
    int fatal;             // if set, all errors are fatal (i.e. during boot)

    /* VIRTUAL METHODS */
    ex_m_write              write;             // ex_write()
    _ex_m_port              port;              // ex_write()
    ex_m_make_pair          make_pair;         // ex_read()
    _ex_m_leaf_to_object    leaf_to_object;    // object wraps
    _ex_m_object_to_leaf    object_to_leaf;
    _ex_m_object_erase_leaf object_erase_leaf;
    _ex_m_object_ref_struct object_ref_struct; // type-checking object_to_leaf
};

#define DEF_ATOM(name)                                          \
    static inline name *object_to_##name(ex *ex, object ob) {   \
        return (name*)object_struct(ex,ob,name##_type()); }

// permanent constant objects
DEF_ATOM(prim)
DEF_ATOM(symbol)

typedef void* (*object_to_pointer)(ex *ex, object);

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

_ _is_vector_type(ex *ex, _ o, long flags);

#define TYPE_ERROR RAISE_TYPE_ERROR
#define ERROR(msg, o) ex_raise_error(EX, SYMBOL(msg), o)
#define INVALID(o) ERROR("invalid", o)
/* Pointer casts (just like predicates) are derived from the
   object_to_pointer function, _except_ for integers: there we use the
   predicate. */
// void* _ex_unwrap_pointer(ex *sc, void *unwrap, object o);
// _ ex_raise_type_error(ex *ex, _ arg_o);
static inline void* _ex_unwrap_pointer(ex *ex, void *unwrap, object o){
    object_to_pointer fn = (object_to_pointer)unwrap;
    void *x = fn(ex, o);
    if (unlikely(!x)) ex_raise_type_error(ex, o);
    return x;
}
long _ex_unwrap_integer(ex *ex, object o);
#define CAST(type,x) ((type*)(_ex_unwrap_pointer(EX, object_to_##type, x)))
#define CAST_INTEGER(x) _ex_unwrap_integer(EX, x)
#define CAST_CHAR(x)    _ex_unwrap_char(EX, x)

#define EXCEPT_ABORT   2 /* highlevel EX abort */
#define EXCEPT_RESTART 3 /* restart primitive (i.e. garbage collection finished) */
#define EXCEPT_HALT    4 /* machine halt */

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
// _ ex_is_pair(ex *ex, _ o);
static inline void _ex_length_rest(ex *ex, _ lst, _ *length, _ *rest) {
    long nb = 0;
    while (object_to_lpair(ex, lst) ||
           object_to_pair(ex, lst)) {
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
    gc_make_tagged(EX->gc, flags, size, __VA_ARGS__)

#define ZERO integer_to_object(0)
#define ONE  integer_to_object(1)

_ _ex_read(ex *ex, port *input_port);




/* Boot file abstraction. */
struct ex_bootinfo;
typedef _ (*_ex_boot_load_t)(ex *ex, struct ex_bootinfo *info);
struct ex_bootinfo {
    _ex_boot_load_t load;  // boot parser, using:
    const char *source;    //  source filename/commands
    int size;              //  length (commands)

    const char *eval;      // to eval after boot
    _ args;                // arguments past to scheme
    int verbose;
};
_ _ex_boot_file(ex *ex,  struct ex_bootinfo *info);
_ _ex_boot_string(ex *ex, struct ex_bootinfo *info);



#define VEC(x) (vector_to_object(((void*)(x))))

#define _ref_struct ex->object_ref_struct
//void *object_aref_struct(ex *, object, void*);
//#define _ref_struct object_aref_struct

#define DEF_LEAF(name) \
    static inline name *object_to_##name(ex *ex, object ob) { \
        return _ref_struct(ex, ob, name##_type());  \
    }

DEF_LEAF(port)
DEF_LEAF(inexact)
DEF_LEAF(bytes)
DEF_LEAF(ck)

typedef char cstring;  // for CAST()
char *object_to_cstring(ex *ex, _ ob);




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


double object_to_double(ex *ex,_ ob);
static inline _ double_to_object(ex *ex, double d) {
    return _ex_leaf_to_object(ex, (leaf_object*)inexact_new(d));
}

typedef struct {
    _ tag;
    _ arg;
} ex_error_info;


#endif
