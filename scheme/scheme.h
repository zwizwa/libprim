#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include "gc_config.h"
#include "symbol.h"
#include "task.h"

typedef struct _scheme sc;
sc *scheme_new(void);

/* The interpreter is written as a step() function manipulating a
   state data structure.  It is based on the CEKS machine from
   http://www.cs.utah.edu/plt/publications/pllc.pdf

   All data structures and function primitives are Scheme values.

   The GC is a stop-and-copy type supporting 4 data types: integers,
   vectors, finalized atoms and unmanaged constants.  Note that it is
   not safe to perform allocation outside of the main interpreter
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

     * ABSTRACT: C code that does not refer to any Scheme data.

   The disadvantage of not being able to access Scheme data from
   impure C code can be largely removed by providing a suspension
   mechanism for primitives.  In this case the C code could behave as
   a coroutine, which allows the use of enumerators / iterators
   instead of construction of intermediate data.

*/


/* The machine will attempt to reduce the current term (which is a
   closure = open term + variable bindings) or continue with the
   computation context (a list of reducible closures). */
typedef struct {
    vector v;
    object redex_or_value;  // naked value or reducible/value closure
    object continuation;    // current continuation
} state;


typedef struct {
    vector v;
    object car;
    object cdr;
} pair;

typedef struct {
    vector v;
    object formals;
    object rest;
    object term;
    object env;
} lambda;

typedef struct {
    vector v;
    object tag;
    object arg;
    object state;
    object prim;
} error;

typedef struct {
    vector v;
    object term;
    object env;
} redex;

typedef struct {
    vector v;
    object datum;
} value;

/* Atoms that need finalization must be wrapped to esure that they
   occur only once in the heap: the finalize() method is called for
   each garbage copy that's encountered. */
typedef struct {
    vector v;
    object atom;
} aref;

/* All continuation frames have a parent frame, and a continuation
   mark dictionary. */
typedef struct {
    vector v;
    object parent;
    object marks;
} k_frame;

/* Arguments are evaluated left to right. */
typedef struct {
    k_frame k;
    object done;   // reversed list of values
    object todo;   // list of redexes
} k_apply;

typedef struct {
    k_frame k;
    object yes;  // non-reduced closures for the 2 branches
    object no;
} k_if;

typedef struct {
    k_frame k;
    object var;
    object env;
    object tl_slot; // state vector slot containing toplevel
} k_set;

/* Sequences are evaluated left to right.  Frame is popped before the
   last redex. */
typedef struct {
    k_frame k;
    object todo;  
} k_seq;

/* Value is transformed into a redex using the env. */
typedef struct {
    k_frame k;
    object env;
} k_macro;




static inline unsigned long vector_to_tag(vector *v) {
    return (v->header) >> GC_VECTOR_TAG_SHIFT;
}

static inline unsigned long object_get_vector_tag(object o){
    vector *v = object_to_vector(o);
    if (!v) return -1;
    return vector_to_tag(v);
}
static inline void vector_set_tag(object o, long tag){
    object_to_vector(o)->header |= (tag << GC_VECTOR_TAG_SHIFT);
}


#define DEF_CAST(type)                                \
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_vector(o); }

// conversion from vector object -> C type
DEF_CAST (pair)
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

/* Global Scheme State*/
#define sc_slot_toplevel        integer_to_object(0)
#define sc_slot_toplevel_macro  integer_to_object(1)
#define sc_slot_state           integer_to_object(2)
#define sc_slot_abort_k         integer_to_object(3)

typedef struct {
    jmp_buf step;  // current CEKS step abort
    object prim;
} scheme_r;

struct _scheme {
    /* Highlevel global state data is accessible from Scheme. */
    object global;

    /* Lowlevel implementation data */

    /* Special form symbol cache */
    object s_lambda;
    object s_begin;
    object s_quote;
    object s_if;
    object s_bang_set;
    object s_letcc;

    /* Objects and classes */
    gc *gc;
    symstore *syms;
    ck_manager *ck_manager;
    atom_class op_prim;

    /* Lowlevel control flow */
    jmp_buf top;       // full interpreter C stack unwind (i.e. for GC)
    long step_entries; // semaphores
    long top_entries;
    scheme_r r;  // saved on step() entry

    /* Temp storage: does not need to be marked. */
    object error_tag;
    object error_arg;
};

static inline symbol* object_to_symbol(object ob, sc *sc) {
    atom *a;
    if ((a = object_to_atom(ob)) && 
        (a->op == &(sc->syms->op)))
        return (symbol*)a;
    else return NULL;
}

/* The ck atoms have a free() finalizer, so need to be wrapped in an
   aref struct */
static inline ck* object_to_ck(object ob, sc *sc) {
    aref *ref;
    atom *a;
    if ((ref = object_to_aref(ob)) &&
        (a = object_to_atom(ref->atom)) &&
        (a->op == (atom_class*)sc->ck_manager))
        return (ck*)a;
    else return NULL;
}

typedef struct {
    atom a;
    void *fn;
    long nargs;
    /* Note: in general it is not allowed to place objects in atom
       structs, but in this case it's a symbol, so will never
       change. */
    object var;
} prim;
static inline long prim_nargs(prim *p){ return p->nargs; }
static inline void *prim_fn(prim *p)  { return p->fn; }
static inline prim* object_to_prim(object ob, sc *sc) {
    atom *a;
    if ((a = object_to_atom(ob)) && 
        (a->op == &(sc->op_prim)))
        return (prim*)a;
    else 
        return NULL;  
}

/* List macros */
#define CAR(o)  object_to_pair(o)->car
#define CDR(o)  object_to_pair(o)->cdr
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))
#define CDDR(o) CDR(CDR(o))
#define CADDR(o) CAR(CDDR(o))

/* Booleans anv void are encoded as constant pointers. */
#define NIL   ((object)0)
#define CONSTANT(x) const_to_object((void*)((((x)<<1)|1)<<GC_TAG_SHIFT))
#define FALSE CONSTANT(0)
#define TRUE  CONSTANT(1)
#define VOID  CONSTANT(2)
#define MT    CONSTANT(3)


/* Scheme primitives */
#define MAX_PRIM_ARGS 3
typedef object (*sc_0)(sc* sc);
typedef object (*sc_1)(sc* sc, object);
typedef object (*sc_2)(sc* sc, object, object);
typedef object (*sc_3)(sc* sc, object, object, object);


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
#define REDEX(t,e)   sc_make_redex(sc,t,e)
#define VALUE(d)     sc_make_value(sc,d)

#define NUMBER(n)     integer_to_object(n)
#define SYMBOL(str)   atom_to_object((atom*)(string_to_symbol(sc->syms, str)))
#define ERROR(msg, o) sc_error(sc, SYMBOL(msg), o)
#define TYPE_ERROR(o) sc_type_error(sc, o)
    
/* Toplevel evaluation */
#define EVAL(expr)    sc_post(sc, _sc_top(sc, expr))

// safe cast to C struct
#define unlikely(x) __builtin_expect((x),0)
typedef void* (*object_to_pointer)(object, sc*);
object sc_type_error(sc *sc, object arg_o);
static inline void* _sc_unwrap(sc *sc, void *_unwrap, sc_1 is, object o) {
    object_to_pointer unwrap = (object_to_pointer)_unwrap;
    if (unlikely(FALSE == is(sc, o))) TYPE_ERROR(o);
    return unwrap(o, sc);
}
#define CAST(type,x) ((type*)(_sc_unwrap(sc, object_to_##type, sc_is_##type, x)))
#define CAST_INTEGER(x) ((integer)(CAST(integer, x)))

// renames
#define sc_make_pair sc_cons

// for geneterated bootstrap code
void _sc_def_prim(sc *sc, object var, void *fn, long nargs);
#define DEF(str,fn,nargs) _sc_def_prim (sc,SYMBOL(str),fn,nargs)


#endif
