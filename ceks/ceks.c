#include <stdlib.h>

#include "symbol.h"
#include "scheme.h"



struct _scheme {
    object state;
    gc *gc;
    symstore *syms;
};

sc *scheme_new(void) {
    sc *x = malloc(sizeof(*x));
    x->gc = gc_new(100);
    x->syms = symstore_new(1000);
    return x;
}

object make_state(sc *sc, object C, object K) {
    object o = gc_vector(sc->gc, 2, C, K);
    vector_set_tag(o, tag_state);
    return o;
}

object make_pair(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, tag_pair);
    return o;
}



/* Some notes on how this is implemented.

 - All datatypes should be Scheme datatypes.  These are implemented in
   terms of gc.h 's object.

*/


/* If we pick evaluation to go from left to right, a continuation
   formed by a hole in the evaluation of (X_1 ... X_n) at position h
   is a list of frames
   
       K = (K_ K_ ...) 

   where each frame 

       K_ = ((V_{h-1} ... V_1) 
             (X_{h+1} ... X_n))

   is a list of values V and a list of closures X.  A closure 

       X = (T E)

   where T is an open term and E is and environment mapping variables
   (identifiers) V to closures X.

       E = ((I X) (I X) ...)
*/


symbol sc_symbol(sc *sc, object o){
    atom *a;
    if ((a = object_atom(o)) &&
        atom_is_symbol(a, sc->syms)) {
        return (symbol)a;
    }
    else {
        return NULL;
    }
}

object close_args(sc *sc, object lst, object E) {
    if (object_null(lst)) return NIL;
    else {
        return 
            CONS(CONS(CAR(lst), E),
                 close_args(sc, lst, E));
    }
}

object find(sc *sc, object E, object var) {
    if (object_null(E)) return NIL;  // FIXME: throw error
    object slot = CAR(E);
    object name = CAR(slot);
    if (name == var) return CDR(slot);
    else find(sc, CDR(E), var);
}

static object step(sc *sc, object _state) {
    state *s = object_state(_state);

    // current expression and environment
    object X = CAR(s->C);  // (open) term
    object E = CDR(s->C);  // environment

    // application
    if (object_is_pair(X)) {
        object X_fn = CAR(X);
        object X_args = CDR(X);
        /* Extend the continuation with a new frame by collecting all
           (open) subterms, bound to the current environment. */
        object C_fn = CONS(X_fn, E);
        object C_args = close_args(sc, X_args, E);
        object K_frame = CONS(NIL, C_args);
        return STATE(C_fn, CONS(K_frame, s->K));
    }
    else {
        // variable reference
        if (sc_symbol(sc, X)) {
            object val = find(sc, E, X);
        }

        // literal
        else {
            // continuation 
            object F   = CAR(s->K); // current frame
            object F_V = CAR(F);    // (reverse) list of reduced values
            object F_C = CDR(F);    // list of closures to evaluate
        
            /* Irreducable values will be placed in the current
               continuation frame, and the next expression to be reduced
               is popped. */
            
            /* If there are no more expressions to be reduced in the
               current frame, an application is performed. */
        }
    }
}
