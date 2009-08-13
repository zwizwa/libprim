#ifndef _PAIR_H_
#define _PAIR_H_

#include "object.h"

typedef struct {
    vector v;
    object car;
    object cdr;
} pair;

/* List macros */
#define CAR(o)  object_to_pair(o)->car
#define CDR(o)  object_to_pair(o)->cdr
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))
#define CDDR(o) CDR(CDR(o))
#define CADDR(o) CAR(CDDR(o))

/* Vectors and pairs are known everywhere. */
#define TAG_VECTOR VECTOR_TAG(0)
#define TAG_PAIR   VECTOR_TAG(1)

DEF_CAST (pair)


#endif
