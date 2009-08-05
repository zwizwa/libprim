#include <stdlib.h>

#include "symbol.h"
#include "scheme.h"



struct _scheme {
    object state;
    gc *gc;
    symstore *syms;
    object s_lambda;
    object s_if;
};

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
object sc_string_to_symbol(sc *sc, const char *str) {
    return (object)string_to_symbol(sc->syms, str);
}

#define SYMBOL(name) sc_string_to_symbol(sc, #name)
#define DEFSYM(name) sc->s_##name = SYMBOL(name)
sc *scheme_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->gc = gc_new(100);
    sc->syms = symstore_new(1000);
    DEFSYM(lambda);
    DEFSYM(if);
    return sc;
}

object make_state(sc *sc, object C, object K) {
    object o = gc_vector(sc->gc, 2, C, K);
    return o;
}
object make_pair(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, tag_pair);
    return o;
}
object make_lambda(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, tag_lambda);
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


object close_args(sc *sc, object lst, object E) {
    if (object_null(lst)) return NIL;
    else return CONS(CONS(CAR(lst), E), close_args(sc, lst, E)); 
}
object sc_reverse(sc *sc, object lst) {
    object rlst = NIL;
    while(!(object_null(lst))) {
        rlst = CONS(CAR(lst), rlst);
        lst  = CDR(lst);
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
    object X = CAR(s->C);  // (open) term
    object E = CDR(s->C);  // environment

    /* Form */
    if (object_is_pair(X)) {
        object X_f = CAR(X);
        object X_args = CDR(X);

        /* Special Form */
        if (sc_symbol(sc, X_f)) {
            if (X_f == sc->s_lambda) {
                object l = LAMBDA(CAR(X_args), CADR(X_args));
                return STATE(CONS(l,E), s->K);
            }
            if (X_f == sc->s_if) {
                // ...
            }
        }

        /* Application Form */
        {
            /* Extend the continuation with a new frame by collecting
               all (open) subterms, and binding them to the current
               environment. */
            object C_fn = CONS(X_f, E);
            object C_args = close_args(sc, X_args, E);
            object K_frame = CONS(NIL, C_args);
            return STATE(C_fn, CONS(K_frame, s->K));
        }
    }
    /* Variable Reference */
    else if (sc_symbol(sc, X)){
        object C = find(sc, E, X);
        return STATE(C, s->K);
    }

    /* Fully reduced value */
    else {
        object F   = CAR(s->K); // current continuation frame
        object F_V = CAR(F);    // (reverse) list of reduced values
        object F_C = CDR(F);    // list of closures to evaluate

        /* If there are remaining closures to evaluate, pop the
           next one and update value list. */
        if (object_pair(F_C)) {
            object C;
            C = CAR(F_C);
            F = CONS(CONS(X, F_V), CDR(F_C));
            return STATE(C, F);
        }
        /* No more expressions to be reduced in the current frame:
           perform application. */
        else {
            object app = sc_reverse(sc, CONS(X, F_V));
            object V_fn   = CAR(app);
            object V_args = CDR(app);

            /* Primitive functions are evaluated. */
            if (object_primitive(V_fn)) {
                primitive p = (primitive)V_fn;
                object rv = p(sc, V_args);
                return STATE(CONS(rv, NIL), // FIXME: can primitives return closures?
                             CDR(s->K));    // drop frame.
            }
            /* Abstraction application extends the environment. */
            else {
                lambda *l = (lambda*)V_fn;
                object formals = l->formals;
                while(!(object_null(formals) || object_null(V_args))) {
                    E = CONS(CONS(CAR(formals), CAR(V_args)), E);
                    formals = CDR(formals);
                    V_args  = CDR(V_args);
                }
                if (!(object_null(formals))) {
                    exit(1); // not enough arguments;
                }
                if (!(object_null(V_args))) {
                    exit(1); // too many arguments;
                }
                return STATE(CONS(l->term, E),  // close term
                             CDR(s->K));        // drop frame
            }
        }
    }
}
