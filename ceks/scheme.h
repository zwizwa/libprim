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
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_vector(o); }

#define TAG_PAIR   1
#define TAG_LAMBDA 2
OBJECT_ACCESS(pair)
OBJECT_ACCESS(state)
OBJECT_ACCESS(lambda)



/* List macros */
#define CAR(o)  object_to_pair(o)->car
#define CDR(o)  object_to_pair(o)->cdr
#define NIL ((object)0)
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))

/* Booleans are encoded as constant pointers. */
#define BOOLVALUE(x) constant_to_object((((x)<<1)|1)<<GC_TAG_SHIFT)
#define TRUE  BOOLVALUE(1)
#define FALSE BOOLVALUE(0)

/* Scheme primitives */
typedef object (*sc_1)(sc* sc, object);



#endif
