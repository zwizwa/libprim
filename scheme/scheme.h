#ifndef _SCHEME_H_
#define _SCHEME_H_

#include <setjmp.h>
#include "gc.h"
#include "symbol.h"

/* The remaining tag is used to represent booleans, which makes it
   possible to _only_ use Scheme primitives in the implementation of
   the interpreter. */

typedef struct _scheme sc;

sc *scheme_new(void);


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

/* The continuation frame is used to sequence the order of closure
   reductions.  Each application encountered during closure reduction
   will create a new list of reducable closures (todo).  One by one
   these will be reduced (done).  Reduction moves from left to right.
   When all are done, the reduced terms are passed to a primitive, or
   an application creates a new reducable closure with reduced
   closures bound to variables.
*/
typedef struct {
    vector v;
    object done;   // reversed list of reduced 
    object todo;   // todo
    object parent; // link
} frame;


typedef struct {
    vector v;
    object car;
    object cdr;
} pair;

typedef struct {
    vector v;
    object formals;
    object term;
} lambda;

/* Syntax. All code is wrapped by a Syntax object. */
typedef struct {
    vector v;
    object datum;
} syntax;


static inline long vector_get_tag(object o){
    vector *v = object_to_vector(o);
    if (!v) return -1;
    return (v->header) >> GC_VECTOR_TAG_SHIFT;
}
static inline void vector_set_tag(object o, long tag){
    object_to_vector(o)->header |= (tag << GC_VECTOR_TAG_SHIFT);
}


#define VECTOR_ACCESS(type)                                \
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_vector(o); }

// conversion from vector object -> C type
VECTOR_ACCESS(pair)
VECTOR_ACCESS(state)
VECTOR_ACCESS(lambda)
VECTOR_ACCESS(closure)
VECTOR_ACCESS(frame)
VECTOR_ACCESS(syntax)

struct _scheme {
    gc *gc;
    symstore *syms;
    object state;
    object toplevel;
    object s_lambda;
    object s_if;

    jmp_buf step;  // current eval step abort
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

// vector tags for interpreter data types
#define TAG_PAIR    1
#define TAG_LAMBDA  2
#define TAG_FRAME   3
#define TAG_STATE   4
#define TAG_CLOSURE 5
#define TAG_SYNTAX  6





/* List macros */
#define CAR(o)  object_to_pair(o)->car
#define CDR(o)  object_to_pair(o)->cdr
#define NIL ((object)0)
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))
#define CDDR(o) CDR(CDR(o))
#define CADDR(o) CAR(CDDR(o))

/* Booleans anv void are encoded as constant pointers. */
#define BOOLVALUE(x) const_to_object((void*)((((x)<<1)|1)<<GC_TAG_SHIFT))
#define VOID  BOOLVALUE(2)
#define TRUE  BOOLVALUE(1)
#define FALSE BOOLVALUE(0)

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
#define SC_EX_TRY   0
#define SC_EX_GC    1  /* step() retestart due to Garbage Collection */
#define SC_EX_ABORT 2  /* full continuation abort */
#define SC_EX_HALT  3  /* machine is in halt state (empty continuation) */


/* Primitives */
#include "scheme_prim.h"

/* Macros valid in sc context. */
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define LAMBDA(f,x)  sc_make_lambda(sc,f,x)
#define CLOSURE(t,e) sc_make_closure(sc,t,e)
#define FRAME(v,c,t) sc_make_frame(sc,v,c,t)
#define SYNTAX(d)    sc_make_syntax(sc,d)

#define NUMBER(n)     integer_to_object(n)
#define SYMBOL(str)   atom_to_object((atom*)(string_to_symbol(sc->syms, str)))
#define ERROR(msg, o) sc_error(sc, SYMBOL(msg), o)
#define TYPE_ERROR(o) ERROR("type",o)
    
/* Toplevel evaluation */
#define EVAL(expr)    sc_post(sc, _sc_eval(sc, expr))

// safe cast to C struct
#define CAST(type,x) object_to_##type(_sc_assert(sc, sc_is_##type, x))




#endif
