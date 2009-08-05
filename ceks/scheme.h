#ifndef _SCHEME_H_
#define _SCHEME_H_

#include "gc.h"
#include "symbol.h"

/* The remaining tag is used to represent booleans, which makes it
   possible to _only_ use Scheme primitives in the implementation of
   the interpreter. */

typedef struct _scheme sc;

sc *scheme_new(void);

typedef struct {
    vector v;
    object C;  // closure
    object K;  // continuation
} state;

typedef struct {
    vector v;
    object car;
    object cdr;
} pair;

typedef struct {
    vector v;
    object expr;
    object env;
} closure;

typedef struct {
    vector v;
    object formals;
    object term;
} lambda;

typedef struct {
    vector v;
    object fn;
    object nargs;
} prim;

static inline long prim_nargs(prim *p){
    return object_to_integer(p->nargs);
}
static inline void *prim_fn(prim *p){
    return object_to_const(p->fn);
}
static inline long vector_get_tag(object o){
    vector *v = object_to_vector(o);
    if (!v) return -1;
    return (v->tag_size) >> GC_VECTOR_TAG_SHIFT;
}
static inline void vector_set_tag(object o, long tag){
    object_to_vector(o)->tag_size |= (tag << GC_VECTOR_TAG_SHIFT);
}


#define OBJECT_ACCESS(type)                                \
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_vector(o); }

// conversion from vector object -> C type
OBJECT_ACCESS(pair)
OBJECT_ACCESS(state)
OBJECT_ACCESS(lambda)
OBJECT_ACCESS(prim)

struct _scheme {
    gc *gc;
    symstore *syms;
    object state;
    object toplevel;
    object s_lambda;
    object s_if;
};

static inline symbol *object_to_symbol(object ob, sc *sc) {
    atom *a;
    if ((a = object_to_atom(ob)) && 
        (a->op == &(sc->syms->op)))
        return (symbol*)a;
    else return NULL;
}

// vector tags for interpreter data types
#define TAG_PAIR   1
#define TAG_LAMBDA 2
#define TAG_PRIM   3



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


/* Primitives */
object sc_interpreter_step(sc*, object);
object sc_make_state(sc*, object, object);
object sc_make_pair(sc*, object, object);
object sc_unsafe_assert(sc *sc, sc_1 predicate, object o);
object sc_write(sc *sc, object o);


/* Macros valid in sc context. */
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define LAMBDA(f,x)  sc_make_lambda(sc,f,x)

#define SYMBOL(str)   atom_to_object((atom*)(string_to_symbol(sc->syms, str)))
#define ERROR(msg, o) sc_error(sc, SYMBOL(msg), o)
#define TYPE_ERROR(o) ERROR("type",o)
    

#endif
