#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include <leaf/class.h>
#include <ex/ex.h>
#include <ex/ex.h_ex_prims>

typedef struct _scheme sc;



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

/* All continuation frames have a parent frame, and a mark dictionary.
   The marks can be used to implement partial continuations, dynamic
   binding, ... as in

   http://people.cs.uchicago.edu/~robby/pubs/papers/icfp2007-fyff.pdf
 */
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


// conversion from vector object -> C type
DEF_STRUCT(state,  TAG_STATE)
DEF_STRUCT(lambda, TAG_LAMBDA)
DEF_STRUCT(redex,  TAG_REDEX)
DEF_STRUCT(error,  TAG_ERROR)
DEF_STRUCT(value,  TAG_VALUE)

DEF_STRUCT(k_apply, TAG_K_APPLY)
DEF_STRUCT(k_if,    TAG_K_IF)
DEF_STRUCT(k_set,   TAG_K_SET)
DEF_STRUCT(k_seq,   TAG_K_SEQ)



/* Global Scheme State*/
#define sc_slot_toplevel        integer_to_object(0)
#define sc_slot_toplevel_macro  integer_to_object(1)
#define sc_slot_state           integer_to_object(2)
#define sc_slot_abort_k         integer_to_object(3)
#define sc_slot_input_port      integer_to_object(4)
#define sc_slot_output_port     integer_to_object(5)
#define sc_slot_error_port      integer_to_object(6)



/* SCHEME */



struct _scheme {
    /* Scheme extends the EX language's memory model, which uncludes
       leaf types and a garbage collector. */
    ex m;

    /* Highlevel global state data is accessible from Scheme. */
    _ global;

    /* Struct to hold errors. */
    _ error;

    /* Lowlevel implementation data.  The object values below don't
       need to be marked because they are short-lived, or constant. */

    /* Special form symbol cache */
    _ s_lambda;
    _ s_begin;
    _ s_quote;
    _ s_if;
    _ s_bang_set;
    _ s_letcc;

};


// SUPER
#define EX (&sc->m)


/* The ck atoms have a free() finalizer, so need to be wrapped in an
   aref struct */
static inline void *object_aref_struct(object ob, ex *m, void *type) {
    aref *ref;
    void *x;
    if ((ref = object_to_aref(ob)) &&
        (x = object_struct(ref->object, type))) return x;
    else return NULL;
}

#define DEF_AREF_TYPE(name)                                            \
    static inline name *object_to_##name(object ob, ex *m) {          \
        return (name*)object_aref_struct(ob,m,m->p->name##_type); }


// GC finalized objects
DEF_AREF_TYPE(ck)
DEF_AREF_TYPE(port)
DEF_AREF_TYPE(bytes)

typedef char cstring;  // for CAST()
static inline char *object_to_cstring(_ ob, ex *m) {
    bytes *b = object_to_bytes(ob, m);
    if (!b) return NULL;
    return cstring_from_bytes(b);
}



static inline long prim_nargs(prim *p){ return p->nargs; }
static inline void *prim_fn(prim *p)  { return p->fn; }



/* ROOT OBJECTS */
#define ROOT_ENV 0

/* Setup */
object _sc_top(sc *sc, object expr);

/* Interpreter exceptions. */
//#define SC_EX_TRY     0
//#define SC_EX_RESTART 1  /* restart from current sc->state. */
//#define SC_EX_ABORT   2  /* abort to default toplevel continuation. */
//#define SC_EX_HALT    3  /* halt leaves the interpreter loop. */
//#define SC_EX_CK      4  /* wrap up C continuation */

/* Macros valid in sc context. */
#define STATE(c,k)   sc_make_state(sc,c,k)
#define REDEX(t,e,m) sc_make_redex(sc,t,e,m)
#define VALUE(d)     sc_make_value(sc,d)

#define NUMBER(n)     integer_to_object(n)
// #define ERROR(msg, o) sc_raise_error(sc, SYMBOL(msg), o)
// #define TYPE_ERROR(o) sc_raise_type_error(sc, o)

#define TYPES         sc->m.p
    
/* Toplevel evaluation */
#define EVAL(expr)    POST(_sc_top((sc*)EX, expr))

// safe cast to C struct
// object sc_raise_type_error(sc *sc, object arg_o);


// renames
#define sc_make_pair sc_cons

// for geneterated bootstrap code
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs);
#define DEF(str,fn,nargs) _sc_def_prim (sc,str,fn,nargs)


_ _sc_make_aref(sc *sc, leaf_object *);
_ _sc_make_symbol(sc *sc, const char *str);
_ _sc_make_string(sc *sc, const char *str);

_ _sc_printf(sc *sc, char *fmt, ...);

// Scheme constants start at 0x100
#define MT    CONSTANT(0x100)




/* INIT */
sc *_sc_new(base_types *types, const char *bootfile);
void _sc_def_prims(sc *sc, prim_def *prims);


#endif
