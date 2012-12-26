#include <zl/layout2D.h>
#include <stdlib.h>

/*

 * Keep memory model abstract: run on top of EX or CELL (or other GC).


 * The layout algorithm goes something like this (from darcs/hgl)

   stacker:  box1, how big are you
   box1:     I'm .... (possibly recursively determined)
   stacker:  box2, how big are you
   ...
   stacker:  parent, I'm this big
   parent:   ok, go here, and shrink/grow
   stacker:  box1, go here and s/g...
   box1:     here's my list of allocated children
   stacker:  box2, go here and s/g ...
   box2:     here's my list of allocated children
   stacker:  parent, here's my list of allocated children
   ...

   This is essentially a 2-pass algorithm with a bottom-up and a
   top-down information flow.

*/


/* In Haskell, the control flow is easily chained using
   continuations/closures.  How to do this in C?  Using a GCd memory
   should make this fairly straighforward. */

// Leaky implementation, just for bootstrapping:
#ifndef CONS
typedef unsigned long word;
typedef int assert_word_size[-1+(sizeof(word) == sizeof(void*))];
struct pair;
typedef struct pair pair;
union value {
    word word;
    pair *pair;
    void *obj;
};
typedef union value value;
struct pair {
    value car;
    value cdr;
};
value cons(value car, value cdr) {
    pair *p = malloc(sizeof(*p));
    p->car = car;
    p->cdr = cdr;
    return (value)p;
}
value car(value cons) { return cons.pair->car; }
value cdr(value cons) { return cons.pair->cdr; }
#define NIL NULL
#define CONS cons
#define CAR car
#define CDR cdr
#endif




