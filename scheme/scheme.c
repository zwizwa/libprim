#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#include "symbol.h"
#include "scheme.h"




#define MARK(field) sc->field = gc_mark(sc->gc, sc->field)
static void mark_roots(sc *sc) {
    MARK(state);
    MARK(toplevel);
}
//static void mark_roots(sc *sc) {
//}


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
object sc_is_integer(sc *sc, object o) {
    if (GC_INTEGER == GC_TAG(o)) return TRUE;
    return FALSE;
}
object sc_is_zero (sc *sc, object o) {
    long i = CAST(integer, o);
    if (i) return FALSE;
    return TRUE;
}
/* Symbols are encoded as GC_ATOM. */
object sc_is_symbol(sc *sc, object o) {
    if(object_to_symbol(o, sc)) return TRUE;
    return FALSE;
}
object sc_is_prim(sc *sc, object o) {
    if(object_to_prim(o,sc)) return TRUE;
    return FALSE;
}


/* The empty list is the NULL pointer */
object sc_is_null(sc *sc, object o) {
    if (!o) return TRUE; else return FALSE;
}
object sc_is_vector(sc *sc, object o){
    vector *v;
    if ((v = object_to_vector(o)) &&
        (0 == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
/* Pairs and lambdas are tagged vectors. */
static object vector_type(object o, long tag) {
    vector *v;
    if ((v = object_to_vector(o)) &&
        (tag == vector_get_tag(o))) { return TRUE; }
    return FALSE;
}
object sc_is_pair(sc *sc, object o)    { return vector_type(o, TAG_PAIR); }
object sc_is_lambda(sc *sc, object o)  { return vector_type(o, TAG_LAMBDA); }
object sc_is_closure(sc *sc, object o) { return vector_type(o, TAG_CLOSURE); }
object sc_is_state(sc *sc, object o)   { return vector_type(o, TAG_STATE); }
object sc_is_frame(sc *sc, object o)   { return vector_type(o, TAG_FRAME); }


object sc_make_pair(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, TAG_PAIR);
    return o;
}
// state + closure don't need to be tagged
object sc_make_state(sc *sc, object C, object K) {
    object o = gc_vector(sc->gc, 2, C, K);
    vector_set_tag(o, TAG_STATE);
    return o;
}
object sc_make_closure(sc *sc, object C, object K) {
    object o = gc_vector(sc->gc, 2, C, K);
    vector_set_tag(o, TAG_CLOSURE);
    return o;
}
object sc_make_lambda(sc *sc, object car, object cdr) {
    object o = gc_vector(sc->gc, 2, car, cdr);
    vector_set_tag(o, TAG_LAMBDA);
    return o;
}
object sc_make_frame(sc *sc, object v, object c, object l) {
    object o = gc_vector(sc->gc, 3, v, c, l);
    vector_set_tag(o, TAG_FRAME);
    return o;
}
// macros bound to sc context
#define CONS(a,b)    sc_make_pair(sc,a,b)
#define STATE(c,k)   sc_make_state(sc,c,k)
#define LAMBDA(f,x)  sc_make_lambda(sc,f,x)





/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
object sc_error(sc *sc, object sym_o, object o) {
    symbol *sym   = object_to_symbol(sym_o, sc);
    if (!sym) sym = string_to_symbol(sc->syms, "error");
    printf("ERROR: ");
    sc_write(sc, sym_o); printf(": ");
    sc_write(sc, o);     printf("\n");
    kill(getpid(), SIGTRAP);
    return NIL;
}
object sc_unsafe_assert(sc *sc, sc_1 predicate, object o) {
    if (FALSE == predicate(sc, o)) { TYPE_ERROR(o); }
    return o;
}
/* Remaining primitives perform type checking. */

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
    return sc_find(sc, sc->toplevel, var);
}
object sc_is_list(sc *sc, object o) {
    if(TRUE==sc_is_null(sc, o)) return TRUE;
    if(FALSE==sc_is_pair(sc, o)) return FALSE;
    return sc_is_list(sc, CDR(o));
}
static object write_vector(sc *sc, char *type, object o) {
    printf("#%s(", type);
    vector *v = object_to_vector(o);
    long i,n = vector_size(v);
    for(i=0;i<n;i++){
        sc_write(sc, v->slot[i]);
        if (i != n-1) printf(" ");
    }
    printf(")");
    return VOID;
}

object sc_write(sc *sc, object o) {
    if(TRUE == sc_is_null(sc, o)) {
        printf("()");
        return VOID;
    }
    if(TRUE == sc_is_list(sc, o)) {
        printf("(");
        for(;;) {
            sc_write(sc, CAR(o));
            o = CDR(o);
            if (TRUE == sc_is_null(sc, o)) {
                printf(")");
                return VOID;
            }
            printf(" ");
        }
    }
    if(TRUE == sc_is_pair(sc, o)) {
        printf("(");
        sc_write(sc, CAR(o));
        printf(" . ");
        sc_write(sc, CDR(o));
        printf(")");
        return VOID;
    }
    if (TRUE == sc_is_symbol(sc, o)) {
        printf("%s", object_to_symbol(o,sc)->name);
        return VOID;
    }
    if (TRUE == sc_is_vector(sc, o))  return write_vector(sc, "", o);
    if (TRUE == sc_is_closure(sc, o)) return write_vector(sc, "closure", o);
    if (TRUE == sc_is_state(sc, o))   return write_vector(sc, "state", o);
    if (TRUE == sc_is_frame(sc, o))   return write_vector(sc, "frame", o);
    if (TRUE == sc_is_lambda(sc, o))  return write_vector(sc, "lambda", o);
    if (TRUE == sc_is_prim(sc, o)) {
        prim *p = object_to_prim(o,sc);
        printf("#prim<%p:%ld>", (void*)(p->fn),p->nargs);
        return VOID;
    }
    if (TRUE == sc_is_integer(sc, o)) {
        printf("%ld", object_to_integer(o));
        return VOID;
    }
    if (TRUE  == o) { printf("#t"); return VOID; }
    if (FALSE == o) { printf("#f"); return VOID; }
    if (VOID  == o) { printf("#<void>"); return VOID; }

    printf("#<%p>",(void*)o);
    return VOID;
}
object sc_post(sc* sc, object o) {
    sc_write(sc, o);
    printf("\n");
    return VOID;
}


object sc_close_args(sc *sc, object lst, object E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(CLOSURE(CAR(lst), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

static inline object run_primitive(sc *sc, void *p, 
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
    closure *c = CAST(closure, s->C);
    object X = c->term;  // (open) term
    object E = c->env;   // environment

    /* Form */
    if (TRUE==sc_is_pair(sc, X)) {
        object X_f = CAR(X);
        object X_args = CDR(X);

        /* Special Form */
        if (TRUE==sc_is_symbol(sc, X_f)) {
            if (X_f == sc->s_lambda) {
                object formals = sc_list_to_vector(sc, CAR(X_args));
                object term = CADR(X_args);
                return STATE(CLOSURE(LAMBDA(formals, term), E),
                             s->K);
            }
            // if (X_f == sc->s_if) {}
            // if (X_f == sc->s_set) {}
        }

        /* Application Form */
        {
            /* Extend the continuation with a new frame by collecting
               all (open) subterms, and binding them to the current
               environment. */
            object C_fn = CLOSURE(X_f, E);
            object C_args = sc_close_args(sc, X_args, E);
            return STATE(C_fn, FRAME(NIL, C_args, s->K));
        }
    }
    /* Variable Reference */
    else if (TRUE==sc_is_symbol(sc, X)){
        object C; 
        if (FALSE == (C = sc_find(sc, E, X))) {
            if (FALSE == (C = sc_find_toplevel(sc, X))) {
                return ERROR("undefined", X);
            }
        }
        return STATE(C, s->K);
    }

    /* Fully reduced value */
    else {
        if (NIL == s->K) return ERROR("halt", o_state);
        frame *f = CAST(frame, s->K);

        /* If there are remaining closures to evaluate, pop the
           next one and push the value to the update value list. */
        if (TRUE==sc_is_pair(sc, f->closures)) {
            return STATE(CAR(f->closures),
                         FRAME(CONS(s->C,f->values),
                               CDR(f->closures), 
                               f->parent));
        }
        /* No more expressions to be reduced in the current frame:
           perform application. */
        else {
            object rargs = CONS(s->C, f->values);
            object p=rargs, C_fn;
            int n = 0;
            while (TRUE==sc_is_pair(sc,p)) {
                n++; C_fn = CAR(p); p = CDR(p);
            }
            // n    == 1 + nb_args
            // V_fn == primitive or lambda

            // unpack the closure
            closure *c = CAST(closure, C_fn);
            object V_fn = c->term;
            object E_fn = c->env;

            /* Primitive functions are evaluated. */
            if (TRUE==sc_is_prim(sc, V_fn)) {
                prim *p = object_to_prim(V_fn,sc);
                if (prim_nargs(p) != (n-1)) {
                    return ERROR("nargs", V_fn);
                }
                /* Perform all allocation _before_ the execution of
                   the primitive.  This is to make sure that we won't
                   be the cause of an abort due to GC _after_ the
                   primitive has executed.  Meaning, primitives won't
                   be restarted unless it's their own fault. */
                object closure = CLOSURE(NIL, NIL);
                object state = STATE(closure, f->parent); // drop frame
                object_to_closure(closure)->term =
                    run_primitive(sc, prim_fn(p), n-1, rargs);
                return state;
            }
            /* Application extends the environment. */
            if (TRUE==sc_is_lambda(sc, V_fn)) {
                lambda *l = CAST(lambda, V_fn);
                vector *v = CAST(vector, l->formals);
                if (vector_size(v) != (n-1)) {
                    return ERROR("nargs", V_fn);
                }
                int i;
                for (i=n-2; i>=0; i--) {
                    E_fn = CONS(CONS(v->slot[i], CAR(rargs)), E_fn);
                    rargs = CDR(rargs);
                }
                return STATE(CLOSURE(l->term, E_fn),  // close term
                             f->parent);              // drop frame
            } 
            else {
                return ERROR("apply", V_fn);
            }
        }
    }
}


// ------------------------

static object make_prim(sc *sc, void *fn, long nargs) {
    prim *p = malloc(sizeof(*p));
    p->a.op = &sc->op_prim; // class
    p->fn = fn;
    p->nargs = nargs;
    return atom_to_object(&p->a);
}
static void define_prim(sc *sc, object var, void *fn, long nargs) {
    object prim = make_prim(sc, fn, nargs);
    sc->toplevel = CONS(CONS(var, CLOSURE(prim, NIL)),
                        sc->toplevel);
}
#define DEFUN(str,fn,nargs) \
    define_prim (sc,SYMBOL(str),fn,nargs)

#define DEFSYM(name) sc->s_##name = SYMBOL(#name)
sc *scheme_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->gc = gc_new(1000000, (gc_mark_roots)mark_roots, sc);
    sc->syms = symstore_new(1000);
    DEFSYM(lambda);
    DEFSYM(if);
    sc->toplevel = NIL;

    DEFUN("null?", sc_is_null, 1);
    DEFUN("zero?", sc_is_zero, 1);

    sc->op_prim.free = NULL;
    
    return sc;
}



