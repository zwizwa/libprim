#ifndef _PAIR_H_
#define _PAIR_H_

typedef struct {
    vector v;
    _ car;
    _ cdr;
} pair;

/* List macros */
#define CAR(o)  object_to_pair(o)->car
#define CDR(o)  object_to_pair(o)->cdr
#define CAAR(o) CAR(CAR(o))
#define CADR(o) CAR(CDR(o))
#define CDDR(o) CDR(CDR(o))
#define CADDR(o) CAR(CDDR(o))



#endif
