#ifndef _SCHEME_H_
#define _SCHEME_H_

#include "gc.h"

/* The remaining tag is used to represent booleans, which makes it
   possible to _only_ use Scheme primitives in the implementation of
   the interpreter. */

typedef struct _scheme sc;
typedef object (*primitive)(sc *sc, object args);


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

static inline long vector_get_tag(object o){
    vector *v = object_vector(o);
    if (!v) return -1;
    return (v->tag_size) >> GC_VECTOR_TAG_SHIFT;
}
static inline void vector_set_tag(object o, long tag){
    object_vector(o)->tag_size |= (tag << GC_VECTOR_TAG_SHIFT);
}

#define OBJECT_ACCESS(type)                             \
    static inline type *object_##type(object o) {       \
        return (type*)object_vector(o); }

#define OBJECT_P(type)                                  \
    static inline int object_is_##type(object o) {      \
        return tag_##type == vector_get_tag(o); }

#define OBJECT(type)             \
    OBJECT_ACCESS(type)          \
    OBJECT_P(type)

#define tag_state  0   /* not tagged */
#define tag_pair   1
#define tag_lambda 2
OBJECT(pair)
OBJECT(state)
OBJECT(lambda)




// macros bound to the machine struct
#define CONS(a,b)      sc_make_pair(sc,a,b)
#define STATE(c,k)     sc_make_state(sc,c,k)
#define LAMBDA(f,x)    sc_make_lambda(sc,f,x)

#define CAR(o)  object_pair(o)->car
#define CDR(o)  object_pair(o)->cdr
#define NIL ((object)0)

#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))

#define BOOLVALUE(x) ((object)(((x)<<GC_TAG_SHIFT)|GC_TAG(GC_BOOL)))
#define TRUE  BOOLVALUE(1)
#define FALSE BOOLVALUE(0)

static inline int object_null(object o) { return (int)o; }


#endif
