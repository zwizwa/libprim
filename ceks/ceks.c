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
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}
object sc_is_integer (sc *sc, object o) {
    if (object_to_integer(o)) return TRUE;
    return FALSE;
}
/* Symbols are encoded as GC_ATOM. */
object sc_is_symbol(sc *sc, object o) {
    atom *a;
    if ((a = object_to_atom(o)) && 
        (a->op == (atom_class*)&sc->syms)) { return TRUE; }
    return FALSE;
}
/* The empty list is the NULL pointer */
object sc_is_null(sc *sc, object o) {
    if (!o) return TRUE; else return FALSE;
}
/* Pairs and lambdas are tagged vectors. */
object sc_is_pair(sc *sc, object o){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (TAG_PAIR == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
object sc_is_lambda(sc *sc, object o){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (TAG_LAMBDA == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
object sc_is_prim(sc *sc, object o){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (TAG_PRIM == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
// assumes object is const string
object sc_unsafe_string_to_symbol(sc *sc, object str) {
    return atom_to_object
        ((atom*)string_to_symbol(sc->syms,
                                 (const char *)
                                 object_to_const(str)));
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
object sc_make_prim(sc *sc, object fn, object nargs) {
    object o = gc_vector(sc->gc, 2, fn, nargs);
    vector_set_tag(o, TAG_PRIM);
    return o;
}

// macros bound to sc context
#define SYMBOL(name) sc_unsafe_string_to_symbol(sc, const_to_object(name))
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define LAMBDA(f,x)  sc_make_lambda(sc,f,x)





/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
object sc_unsafe_error(sc *sc, object sym_o, object o) {
    symbol *sym = object_to_symbol(sym_o);
    fprintf(stderr, "ERROR: %s %p\n", sym->name, (void*)o);
    exit(1);
    return NIL;
}
/* Remaining primitives perform type checking. */
#define ERROR(msg, o) \
    sc_unsafe_error                             \
    (sc, sc_unsafe_string_to_symbol             \
     (sc, const_to_object(msg)), o)
#define TYPE_ERROR(o) ERROR("type",o)
    
static object sc_unsafe_assert(sc *sc, sc_1 predicate, object o) {
    if (FALSE == predicate(sc, o)) { TYPE_ERROR(o); }
    return o;
}
// safe cast to C struct
#define CAST(type,x) \
    object_to_##type(sc_unsafe_assert(sc, sc_is_##type, x))

object sc_make_vector(sc *sc, object slots) {
    return gc_vector(sc->gc, CAST(integer, slots));
}
object sc_reverse(sc *sc, object lst) {
    object rlst = NIL;
    while(FALSE == (sc_is_null(sc, lst))) {
        pair *p = CAST(pair, lst);
        rlst = CONS(p->car, rlst);
        lst  = p->cdr;
    }
    return rlst;
}
object sc_length(sc *sc, object lst) {
    int nb = 0;
    while (TRUE == sc_is_pair(sc, lst)) { nb++; lst = CDR(lst); }
    if (FALSE == sc_is_null(sc, lst)) TYPE_ERROR(lst);
    return integer_to_object(nb);
}
object sc_close_args(sc *sc, object lst, object E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(CONS(CAR(lst), E), sc_close_args(sc, lst, E)); 
}
object sc_list_to_vector(sc *sc, object lst){
    object slots = sc_length(sc, lst);
    object vo = sc_make_vector(sc, slots);
    vector *v = object_to_vector(vo);
    long i=0;
    while (FALSE == sc_is_null(sc, lst)) {
        v->slot[i++] = CAR(lst);
        lst = CDR(lst);
    }
    return vo;
}

object sc_find(sc *sc, object E, object var) {
    if (TRUE == sc_is_null(sc, E)) {
        return FALSE;
    }
    object slot = CAR(E);
    object name = CAR(slot);
    if (name == var) return CDR(slot);
    else return sc_find(sc, CDR(E), var);
}
object sc_find_toplevel(sc *sc, object var) {
    vector *v = object_to_vector(sc->gc->root);
    return sc_find(sc, v->slot[ROOT_ENV], var);
}




static inline object apply_primitive(sc *sc, void *p, 
                                     int nargs, object ra) {
    switch(nargs) {
    case 0: return ((sc_0)p)(sc);
    case 1: return ((sc_1)p)(sc, CAR(ra));
    case 2: return ((sc_2)p)(sc, CADR(ra), CAR(ra));
    case 3: return ((sc_3)p)(sc, CADDR(ra), CADR(ra), CAR(ra));
    default:
        return ERROR("prim", integer_to_object(nargs));
    }
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

object sc_interpreter_step(sc *sc, object o_state) {
    state *s = object_to_state(o_state);
    object X = CAR(s->C);  // (open) term
    object E = CDR(s->C);  // environment

    /* Form */
    if (TRUE==sc_is_pair(sc, X)) {
        object X_f = CAR(X);
        object X_args = CDR(X);

        /* Special Form */
        if (TRUE==sc_is_symbol(sc, X_f)) {
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
    else if (TRUE==sc_is_symbol(sc, X)){
        object C = sc_find(sc, E, X);
        return STATE(C, s->K);
    }

    /* Fully reduced value */
    else {
        object F   = CAR(s->K); // current continuation frame
        object F_V = CAR(F);    // (reverse) list of reduced values
        object F_C = CDR(F);    // list of closures to evaluate

        F_V = CONS(X, F_V);

        /* If there are remaining closures to evaluate, pop the
           next one and update value list. */
        if (TRUE==sc_is_pair(sc, F_C)) {
            object C;
            C = CAR(F_C);
            F = CONS(F_V, CDR(F_C));
            return STATE(C, F);
        }
        /* No more expressions to be reduced in the current frame:
           perform application. */
        else {
            object p=F_V, V_fn;
            int n = 0;
            while (FALSE==sc_is_null(sc,p)) {
                n++; V_fn = CAR(p); p = CDR(p);
            }
            // n    == 1 + nb_args
            // V_fn == primitive or lambda

            /* Primitive functions are evaluated. */
            if (TRUE==sc_is_prim(sc, V_fn)) {
                prim *p = object_to_prim(V_fn);
                if (prim_nargs(p) != (n-1)) {
                    return ERROR("nargs", V_fn);
                }
                object rv = apply_primitive
                    (sc, prim_fn(p), n-1, F_V);
                return STATE(CONS(rv, NIL), 
                             CDR(s->K));    // drop frame.
            }
            /* Abstraction application extends the environment. */
            else {
                lambda *l = (lambda*)V_fn;
                vector *v = object_to_vector(l->formals);
                if (vector_size(v) != (n-1)) {
                    return ERROR("nargs", V_fn);
                }
                int i;
                for (i=n-2; i>=0; i++) {
                    E = CONS(CONS(v->slot[i], CAR(F_V)), E);
                    F_V = CDR(F_V);
                }
                return STATE(CONS(l->term, E),  // close term
                             CDR(s->K));        // drop frame
            }
        }
    }
}


// ------------------------

#define DEFSYM(name) sc->s_##name = SYMBOL(#name)
sc *scheme_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->gc = gc_new(100, NULL, NULL);
    sc->syms = symstore_new(1000);
    DEFSYM(lambda);
    DEFSYM(if);
    return sc;
}



