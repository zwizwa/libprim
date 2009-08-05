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


/* To simplify the implementation _all_ functions are Scheme
   primitives operating on tagged values.  They are named sc_xxx for
   ease of lifting them from the source file for environment
   bootstrap.

   The sc_unsafe_xxx functions operate on arguments without checking
   them.  These are useful internally when types are known to be
   correct.
*/

/* Booleans are GC_CONST */
object sc_is_bool (sc *sc, object o) {
    void *x;
    if ((x = object_to_constant(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}

/* Symbols are encoded as GC_ATOM. */
object sc_is_symbol(sc *sc, object o) {
    atom *a;
    if ((a = object_to_atom(o)) && 
        (a->op == (atom_class)&sc->syms)) { return TRUE; }
    return FALSE;
}
/* The empty list is the NULL pointer */
object sc_is_null(sc *sc, object o) {
    if (!o) return TRUE; else return FALSE;
}
/* Pairs and lambdas are tagged vectors. */
object sc_is_pair(sc *sc, object ob){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (TAG_PAIR == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
object sc_is_lambda(sc *sc, object ob){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (TAG_LAMBDA == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
// assumes object is const string
object sc_unsafe_string_to_symbol(sc *sc, object str) {
    return atom_to_object
        (atom*)string_to_symbol(sc->syms,
                                (const char *)
                                object_to_constant(str));
}
object sc_make_pair(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, TAG_PAIR);
    return o;
}
// state vector doesn't need to be tagged
object sc_make_state(sc *sc, object C, object K) {
    object o = gc_vector(sc->gc, 2, C, K);
    return o;
}
object sc_make_lambda(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, TAG_LAMBDA);
    return o;
}

// macros bound to sc context
#define SYMBOL(name) sc_string_to_symbol_unsafe(sc, #name)
#define DEFSYM(name) sc->s_##name = SYMBOL(name)
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define LAMBDA(f,x)  sc_make_lambda(sc,f,x)





/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
object sc_error(sc *sc, object sym, object o) {
    fprintf(stderr, "ERROR: %s\n", ((symbol)object_to_atom(sym))->name);
    exit(1);
    return NIL;
}
/* Remaining primitives perform type checking. */
static object sc_unsafe_assert(sc *sc, sc_1 predicate, object o) {
    if (FALSE == predicate(sc, o)) {
        sc_error(sc, sc_unsafe_string_to_symbol(sc, "type"), o);
    }
    return o;
}
// safe cast to C struct
#define CAST(type,x) \
    object_to_##type(sc_unsafe_assert(sc, sc_is_##type, x)

object sc_make_vector(sc *sc, object slots) {
    return gc_vector(sc->gc, CAST(integer, slots));
}
object sc_reverse(sc *sc, object lst) {
    object rlst = NIL;
    while(!(object_is_null(lst))) {
        pair *p = CAST(pair, lst);
        rlst = CONS(p->car, rlst);
        lst  = p->cdr;
    }
    return rlst;
}
object sc_length(sc *sc, object lst) {
    int nb = 0;
    while (object_pair(lst)) { nb++; lst = CDR(lst); }
    void *x = CAST(null, lst); // make sure it's a proper list
    return integer_object(nb);
}

// ------------------------

sc *scheme_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->gc = gc_new(100);
    sc->syms = symstore_new(1000);
    DEFSYM(lambda);
    DEFSYM(if);
    return sc;
}


object sc_close_args(sc *sc, object lst, object E) {
    if (object_null(lst)) return NIL;
    else return CONS(CONS(CAR(lst), E), sc_close_args(sc, lst, E)); 
}
object sc_list_to_vector(sc *sc, object lst){
    long slots = object_integer(sc_list_length(sc, lst));
    object vo = sc_make_vector(sc, integer_object(slots));
    vector *v = object_vector(vo);
    long i=0;
    while (!(object_null(lst))) {
        v->slot[i++] = CAR(lst);
        lst = CDR(lst);
    }
    return vo;
}
object sc_find(sc *sc, object E, object var) {
    if (object_null(E)) return NIL;  // FIXME: throw error
    object slot = CAR(E);
    object name = CAR(slot);
    if (name == var) return CDR(slot);
    else find(sc, CDR(E), var);
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
                object formals = sc_list_to_vector(sc, CAR(X_args));
                object term = CADR(X_args);
                return STATE(CONS(LAMBDA(formals, term),
                                  E), s->K);
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
            object C_args = sc_close_args(sc, X_args, E);
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
