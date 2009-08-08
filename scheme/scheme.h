#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include "gc.h"
#include "symbol.h"

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
   computation context (a list of reducable closures). */
typedef struct {
    vector v;
    object closure;      // current (reducable) closure
    object continuation; // current continuation
} state;

typedef struct {
    vector v;
    object term;  // (reducable) (open) term
    object env;   // term's free variables
} closure;

/* The k_apply continuation frame is used to sequence the order of
   reductions.  Each application will create a new list of reducable
   closures, which will be reduced one by one from left to right.
   When done, the reduced terms are passed to a primitive, or an
   application creates a new reducable closure with reduced closures
   bound to variables. */

typedef struct {
    vector v;
    object done;   // reversed list of reduced 
    object todo;   // todo
    object parent; // link
} k_apply;

typedef struct {
    vector v;
    object yes;  // non-reduced closures for the 2 branches
    object no;
    object parent;
} k_if;

typedef struct {
    vector v;
    object var;  
    object parent;
} k_set;

typedef struct {
    vector v;
    object todo;  
    object parent;
} k_seq;

typedef struct {
    vector v;
    object env;
    object parent;
} k_macro;

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
} lambda;

typedef struct {
    vector v;
    object tag;
    object arg;
    object state;
} error;

/* All reducable code is wrapped by an AST object. */
typedef struct {
    vector v;
    object datum;
} ast;


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
DEF_CAST (closure)
DEF_CAST (ast)
DEF_CAST (error)

DEF_CAST (k_apply)
DEF_CAST (k_if)
DEF_CAST (k_set)
DEF_CAST (k_seq)
DEF_CAST (k_macro)

struct _scheme {
    gc *gc;
    symstore *syms;

    object state;
    object state_abort;
    object toplevel;
    object toplevel_macro;

    object error_tag;
    object error_arg;

    object s_lambda;
    object s_quote;
    object s_if;
    object s_bang_set;
    object s_begin;
    object s_letcc;

    jmp_buf step;   // current CEKS step abort
    jmp_buf run;    // full interpreter C stack unwind (i.e. for GC)

    atom_class op_prim;
    long entries;
};

static inline symbol* object_to_symbol(object ob, sc *sc) {
    atom *a;
    if ((a = object_to_atom(ob)) && 
        (a->op == &(sc->syms->op)))
        return (symbol*)a;
    else return NULL;
}

typedef struct {
    atom a;
    void *fn;
    long nargs;
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
#define NIL ((object)0)
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))
#define CDDR(o) CDR(CDR(o))
#define CADDR(o) CAR(CDDR(o))

/* Booleans anv void are encoded as constant pointers. */
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
void   _sc_run(sc *sc);
object _sc_eval(sc *sc, object expr);
sc    *_sc_new(void);

/* Interpreter exceptions. */
#define SC_EX_TRY     0
#define SC_EX_RESTART 1  /* restart from current sc->state. */
#define SC_EX_ABORT   2  /* abort to default toplevel continuation. */
#define SC_EX_HALT    3  /* halt leaves the interpreter loop. */

/* Macros valid in sc context. */
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define CLOSURE(t,e) sc_make_closure(sc,t,e)
#define AST(d)       sc_make_ast(sc,d)


#define NUMBER(n)     integer_to_object(n)
#define SYMBOL(str)   atom_to_object((atom*)(string_to_symbol(sc->syms, str)))
#define ERROR(msg, o) sc_error(sc, SYMBOL(msg), o)
#define TYPE_ERROR(o) ERROR("type",o)
    
/* Toplevel evaluation */
#define EVAL(expr)    sc_post(sc, _sc_eval(sc, expr))

// safe cast to C struct
#define CAST(type,x) object_to_##type(_sc_assert(sc, sc_is_##type, x))

// renames
#define sc_make_pair sc_cons


// Geneterated bootstrap code
void _sc_def_prim(sc *sc, object var, void *fn, long nargs);
#define DEF(str,fn,nargs) _sc_def_prim (sc,SYMBOL(str),fn,nargs)
#include "scheme.h_"


#endif
