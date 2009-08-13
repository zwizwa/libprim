#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include "symbol.h"
#include "task.h"
#include "port.h"
#include "bytes.h"
#include "pair.h"
#include "gc.h"


typedef struct _scheme sc;
sc *scheme_new(void);

/* The interpreter is written as a step() function manipulating a
   state data structure.  It is based on the CEKS machine from
   http://www.cs.utah.edu/plt/publications/pllc.pdf

   All data structures and function primitives are Scheme values.

   The GC is a stop-and-copy type Cheney algorithm supporting 4 data
   types: integers, vectors, finalizers and constants.  Note that it
   is not safe to perform allocation outside of the main interpreter
   loop, and the main loop is not re-entrant.

   When GC is triggered in the context of a primitive, it will be
   aborted and restarted.  Therefore primitives may not perform vector
   allocation _after_ performing side effects (i.e. mutation of
   globally accessible state, be it interpreter state or foreign
   data).

   Such an interpreter can support two kinds of primitives:

     * RESTARTABLE: Pure functions operating on Scheme data structures
       (or impure functions that do not perform allocation _after_
       mutation).

     * ABSTRACT: C code that does not refer to any Scheme data, and
       thus cannot trigger GC.

   The disadvantage of not being able to access Scheme data from
   impure C code can be largely removed by providing a suspension
   mechanism for primitives.  In this case the C code could behave as
   a coroutine, which allows the use of enumerators / iterators
   instead of construction of intermediate (Scheme) data structures.

*/


typedef object _;  // Highly effective noise reduction.


/* The machine will attempt to reduce the current term (which is a
   closure = open term + variable bindings) or continue with the
   computation context (a list of reducible closures). */
typedef struct {
    vector v;
    _ redex_or_value;  // naked value or reducible/value closure
    _ continuation;    // current continuation
} state;


typedef struct {
    vector v;
    _ formals;
    _ rest;
    _ term;
    _ env;
    _ menv;
} lambda;

typedef struct {
    vector v;
    _ tag;
    _ arg;
    _ state;
    _ prim;
} error;

typedef struct {
    vector v;
    _ term;
    _ env;
    _ menv;
} redex;

typedef struct {
    vector v;
    _ datum;
} value;

/* Atoms that need finalization must be wrapped to esure that they
   occur only once in the heap: the finalize() method is called for
   each garbage copy that's encountered. */
typedef struct {
    vector v;
    _ fin;
    _ atom;
} aref;

/* All continuation frames have a parent frame, and a continuation
   mark dictionary. */
typedef struct {
    vector v;
    _ parent;
    _ marks;
} k_frame;

/* Arguments are evaluated left to right.  In retrospect it would have
   been simpler to evaluate from right to left: this makes it easier
   to use k_apply continuations for other purposes. */
typedef struct {
    k_frame k;
    _ done;   // reversed list of values
    _ todo;   // list of redexes
} k_apply;

typedef struct {
    k_frame k;
    _ yes;  // non-reduced closures for the 2 branches
    _ no;
} k_if;

typedef struct {
    k_frame k;
    _ var;
    _ env;
    _ tl_slot; // state vector slot containing toplevel
} k_set;

/* Sequences are evaluated left to right.  Frame is popped before the
   last redex. */
typedef struct {
    k_frame k;
    _ todo;  
} k_seq;

/* Value is transformed into a redex using the env. */
typedef struct {
    k_frame k;
    _ env;
    _ menv;
} k_macro;



// conversion from vector object -> C type
DEF_CAST (state)
DEF_CAST (lambda)
DEF_CAST (redex)
DEF_CAST (error)
DEF_CAST (value)
DEF_CAST (aref)

DEF_CAST (k_apply)
DEF_CAST (k_if)
DEF_CAST (k_set)
DEF_CAST (k_seq)
DEF_CAST (k_macro)


/* primitive function wrapper */
typedef struct {
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


/* Global Scheme State*/
#define sc_slot_toplevel        integer_to_object(0)
#define sc_slot_toplevel_macro  integer_to_object(1)
#define sc_slot_state           integer_to_object(2)
#define sc_slot_abort_k         integer_to_object(3)
#define sc_slot_debug_port      integer_to_object(4)

typedef struct {
    jmp_buf step;  // current CEKS step abort
    _ prim;
} scheme_r;

struct _scheme {
    /* Highlevel global state data is accessible from Scheme. */
    _ global;

    /* Lowlevel implementation data.  The object values below don't
       need to be marked because they are short-lived, or constant. */

    /* Special form symbol cache */
    _ s_lambda;
    _ s_begin;
    _ s_quote;
    _ s_if;
    _ s_bang_set;
    _ s_letcc;

    /* Primitive datatypes. */
    symbol_class *symbol_type;
    ck_class *ck_type;
    prim_class *prim_type;
    port_class *port_type;
    bytes_class *bytes_type;

    /* Delegate objects. */
    gc *gc;

    /* Lowlevel control flow */
    jmp_buf top;       // full interpreter C stack unwind (i.e. for GC)
    long step_entries; // semaphores
    long top_entries;
    scheme_r r;  // saved on step() entry

    /* Temp storage: does not need to be marked. */
    _ error_tag;
    _ error_arg;
};




/* The ck atoms have a free() finalizer, so need to be wrapped in an
   aref struct */
static inline void *object_aref_struct(object ob, sc *sc, void *type) {
    aref *ref;
    void *x;
    if ((ref = object_to_aref(ob)) &&
        (x = object_struct(ref->atom, type))) return x;
    else return NULL;
}

#define DEF_AREF_TYPE(name)                                            \
    static inline name *object_to_##name(object ob, sc *sc) {          \
        return (name*)object_aref_struct(ob,sc,sc->name##_type); }
#define DEF_CONST_TYPE(name,classlist)                                 \
    static inline name *object_to_##name(object ob, sc *sc) {          \
        return (name*)object_struct(ob,sc->name##_type); }



// GC finalized objects
DEF_AREF_TYPE(ck)
DEF_AREF_TYPE(port)
DEF_AREF_TYPE(bytes)

// permanent constant objects
DEF_CONST_TYPE(prim)
DEF_CONST_TYPE(symbol)




static inline long prim_nargs(prim *p){ return p->nargs; }
static inline void *prim_fn(prim *p)  { return p->fn; }

/* Scheme primitives */
#define MAX_PRIM_ARGS 3
typedef _ (*sc_0)(sc* sc);
typedef _ (*sc_1)(sc* sc, _);
typedef _ (*sc_2)(sc* sc, _, _);
typedef _ (*sc_3)(sc* sc, _, _, _);


/* ROOT OBJECTS */
#define ROOT_ENV 0

/* Setup */
object _sc_top(sc *sc, object expr);
sc    *_sc_new(void);

/* Interpreter exceptions. */
#define SC_EX_TRY     0
#define SC_EX_RESTART 1  /* restart from current sc->state. */
#define SC_EX_ABORT   2  /* abort to default toplevel continuation. */
#define SC_EX_HALT    3  /* halt leaves the interpreter loop. */
#define SC_EX_CK      4  /* wrap up C continuation */

/* Macros valid in sc context. */
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define REDEX(t,e,m) sc_make_redex(sc,t,e,m)
#define VALUE(d)     sc_make_value(sc,d)

#define NUMBER(n)     integer_to_object(n)
#define SYMBOL(str)   _sc_make_symbol(sc, str)
#define STRING(str)   _sc_make_string(sc, str)
#define ERROR(msg, o) sc_error(sc, SYMBOL(msg), o)
#define TYPE_ERROR(o) sc_type_error(sc, o)
    
/* Toplevel evaluation */
#define EVAL(expr)    sc_post(sc, _sc_top(sc, expr))

// safe cast to C struct
typedef void* (*object_to_pointer)(object, sc*);
object sc_type_error(sc *sc, object arg_o);

/* Pointer casts (just like predicates) are derived from the
   object_to_pointer function, _except_ for integers: there we use the
   predicate. */
static inline void* _sc_unwrap_pointer(sc *sc, void *unwrap, object o){
    void *x = ((object_to_pointer)unwrap)(o, sc);
    if (unlikely(!x)) TYPE_ERROR(o);
    return x;
}
_ sc_is_integer(sc*, _);
static inline long _sc_unwrap_integer(sc *sc, object o) {
    if ((FALSE == sc_is_integer(sc, o))) return TYPE_ERROR(o);
    return object_to_integer(o);
}
#define CAST(type,x) ((type*)(_sc_unwrap_pointer(sc, object_to_##type, x)))
#define CAST_INTEGER(x) _sc_unwrap_integer(sc, x)

// renames
#define sc_make_pair sc_cons

// for geneterated bootstrap code
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs);
#define DEF(str,fn,nargs) _sc_def_prim (sc,str,fn,nargs)

#define STRUCT(flags, size, ...) \
    return _sc_make_tagged_struct(sc, flags, size, __VA_ARGS__)
static inline object _sc_make_tagged_struct(sc *sc, long flags, long slots, ...) {
    va_list ap;
    va_start(ap, slots);
    object o = gc_make_v(sc->gc, slots, ap);
    va_end(ap);   
    vector_set_flags(object_to_vector(o), flags);
    return o;
}


_ _sc_make_aref(sc *sc, void *fin, void *ptr);
_ _sc_make_symbol(sc *sc, const char *str);
_ _sc_make_string(sc *sc, const char *str);

_ _sc_printf(sc *sc, char *fmt, ...);

// Scheme constants start at 0x100
#define MT    CONSTANT(0x100)

#endif
