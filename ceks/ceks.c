#include <stdlib.h>

#include "gc.h"
#include "scheme.h"



struct _scheme {
    gc *gc;
};

sc *scheme_new(void) {
    sc *x = malloc(sizeof(*x));
    x->gc = gc_new(100);
    return x;
}


object make_machine(sc *sc, object C, object E, object K, object S) {
    object v = gc_vector(sc->gc, 4, C, E, K, S);
    return v;
}
machine *object_machine(object o) {
    return (machine*)object_vector(o);
}





/* Some notes on how this is implemented.

 - All datatypes should be Scheme datatypes.  These are implemented in
   terms of gc.h 's object.

*/

// ------------


/* A closure is a pair of ( open-term . environment ) 
   An environment is a function mapping variables to closures.

   Chapter 6 Machines, p 74 in PLLC.
*/

#define CONT_FUN 1
#define CONT_ARG 2
#define CONT_OPD 3
typedef struct {
    int tag;
    
} cont;

