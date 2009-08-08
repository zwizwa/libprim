#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "symbol.h"
#include "scheme.h"




/* --- PRIMITIVES --- */

/* To simplify the implementation, as much as possible functions are
   implemented as Scheme primitives operating on tagged values.  They
   are named sc_xxx for ease of lifting them from the source file for
   environment bootstrap.

   The functions operating on *sc that are too lowlevel to respect the
   sc_xxx ABI (because they use values that cannot be represented as a
   Scheme object, or because they violate behavioural constraints) are
   named _sc_xxx.  These are kept to a minimum to avoid duplication.

*/

static _ _sc_assert(sc *sc, sc_1 predicate, _ o) {
    if (FALSE == predicate(sc, o)) { TYPE_ERROR(o); }
    return o;
}

/* Booleans are GC_CONST */
_ sc_is_bool (sc *sc, _ o) {
    void *x;
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}
_ sc_is_integer(sc *sc, _ o) {
    if (GC_INTEGER == GC_TAG(o)) return TRUE;
    return FALSE;
}
_ sc_is_zero (sc *sc, _ o) {
    long i = CAST(integer, o);
    if (i) return FALSE;
    return TRUE;
}
/* Symbols are encoded as GC_ATOM. */
_ sc_is_symbol(sc *sc, _ o) {
    if(object_to_symbol(o, sc)) return TRUE;
    return FALSE;
}
_ sc_is_prim(sc *sc, _ o) {
    if(object_to_prim(o,sc)) return TRUE;
    return FALSE;
}


/* The empty list is the NULL pointer */
_ sc_is_null(sc *sc, _ o) {
    if (!o) return TRUE; else return FALSE;
}
/* Pairs and lambdas are tagged vectors. */
static _ vector_type(_ o, long tag) {
    vector *v;
    if ((v = object_to_vector(o)) &&
        (tag == vector_to_tag(v))) { return TRUE; }
    return FALSE;
}

static _ _sc_make_struct(sc *sc, long tag, long slots, ...) {
    va_list ap;
    va_start(ap, slots);
    _ o = gc_vector_v(sc->gc, slots, ap);
    va_end(ap);   
    vector_set_tag(o, tag);
    return o;
}


// vector tags for interpreter data types
#define TAG_VECTOR    0

#define TAG_PAIR      1
#define TAG_LAMBDA    2
#define TAG_STATE     3
#define TAG_CLOSURE   4
#define TAG_REDEX     5
#define TAG_ERROR     6

#define TAG_K_IF      8
#define TAG_K_SET     9
#define TAG_K_APPLY  10
#define TAG_K_SEQ    11
#define TAG_K_MACRO  12
#define TAG_K_IGNORE 13
// reserved 14 15
static inline long tag_is_k(long tag) {
    return (0x8L == (tag & (~0x7L)));
}


typedef _ o;  // This file is quasi-Huffman encoded ;)

/* Predicates */
_ sc_is_vector(sc *sc, _ o)  { return vector_type(o, TAG_VECTOR); }
_ sc_is_pair(sc *sc, _ o)    { return vector_type(o, TAG_PAIR); }
_ sc_is_lambda(sc *sc, _ o)  { return vector_type(o, TAG_LAMBDA); }
_ sc_is_closure(sc *sc, _ o) { return vector_type(o, TAG_CLOSURE); }
_ sc_is_state(sc *sc, _ o)   { return vector_type(o, TAG_STATE); }
_ sc_is_redex(sc *sc, _ o)   { return vector_type(o, TAG_REDEX); }
_ sc_is_error(sc *sc, _ o)   { return vector_type(o, TAG_ERROR); }

_ sc_is_k_if(sc *sc, _ o)    { return vector_type(o, TAG_K_IF); }
_ sc_is_k_apply(sc *sc, _ o) { return vector_type(o, TAG_K_APPLY); }
_ sc_is_k_seq(sc *sc, _ o)   { return vector_type(o, TAG_K_SEQ); }
_ sc_is_k_set(sc *sc, _ o)   { return vector_type(o, TAG_K_SET); }
_ sc_is_k_macro(sc *sc, _ o) { return vector_type(o, TAG_K_MACRO); }

_ sc_is_k(sc *sc, _ o) {
    vector *v;
    if (MT == o) return TRUE;
    if ((v = object_to_vector(o))) {
        if (tag_is_k(vector_to_tag(v))) return TRUE;
    }
    return FALSE;
}
_ sc_k_parent(sc *sc, _ o) {
    vector *v;
    if (MT == o) TYPE_ERROR(o);
    if ((v = object_to_vector(o))) {
        if (tag_is_k(vector_to_tag(v))) return v->slot[0];
    }
    return TYPE_ERROR(o);
}

#define STRUCT(tag, size, ...) return _sc_make_struct(sc, tag, size, __VA_ARGS__)

/* Constructors */
// C = closure
// K = continuation
// F = formal argument vector
// R = rest args
// S = syntax term (REDEX)
// D = done (list of reduced closures)
// T = todo (list of non-reduced closures)
// E = environment
// P = parent continuation
// D = datum



_ sc_cons(sc *sc, _ car, _ cdr)          {STRUCT(TAG_PAIR,    2, car,cdr);}
_ sc_make_state(sc *sc, _ C, _ K)        {STRUCT(TAG_STATE,   2, C,K);}
_ sc_make_closure(sc *sc, _ T, _ E)      {STRUCT(TAG_CLOSURE, 2, T,E);}
_ sc_make_lambda(sc *sc, _ F, _ R, _ S)  {STRUCT(TAG_LAMBDA , 3, F,R,S);}
_ sc_make_error(sc *sc, _ T, _ A, _ K)   {STRUCT(TAG_ERROR,   3, T,A,K);}
_ sc_make_redex(sc *sc, _ D)             {STRUCT(TAG_REDEX,   1, D);}

// 'P' is in slot 0
_ sc_make_k_apply(sc *sc, _ P, _ D, _ T) {STRUCT(TAG_K_APPLY,  3, P,D,T);}
_ sc_make_k_if(sc *sc, _ P, _ Y, _ N)    {STRUCT(TAG_K_IF,     3, P,Y,N);}
_ sc_make_k_set(sc *sc, _ P, _ V)        {STRUCT(TAG_K_SET,    2, P,V);}
_ sc_make_k_seq(sc *sc, _ P, _ T)        {STRUCT(TAG_K_SEQ,    2, P,T);}
_ sc_make_k_macro(sc *sc, _ P, _ E)      {STRUCT(TAG_K_MACRO,  2, P,E);}


_ sc_car(sc *sc, _ o) { pair *p = CAST(pair, o); return p->car; }
_ sc_cdr(sc *sc, _ o) { pair *p = CAST(pair, o); return p->cdr; }

/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
_ sc_trap(sc *sc) {
    kill(getpid(), SIGTRAP);
    return VOID;
}
_ sc_error(sc *sc, _ sym_o, _ arg_o) {
    sc->error_tag = sym_o;
    sc->error_arg = arg_o;
    // if (sym_o != SYMBOL("halt")) sc_trap(sc);
    longjmp(sc->step, SC_EX_ABORT);
}
_ sc_make_vector(sc *sc, _ slots, _ init) {
    long i,n = CAST(integer, slots);
    _ o = gc_alloc(sc->gc, n);
    vector *v = object_to_vector(o);
    for(i=0; i<n; i++) v->slot[i] = init;
    return o;
}
_ sc_reverse(sc *sc, _ lst) {
    _ rlst = NIL;
    while(FALSE == (sc_is_null(sc, lst))) {
        pair *p = CAST(pair, lst);
        rlst = CONS(p->car, rlst);
        lst  = p->cdr;
    }
    return rlst;
}

// parse improper list
static void _sc_length_rest(sc *sc, _ lst, _ *length, _ *rest) {
    long nb = 0;
    while (TRUE == sc_is_pair(sc, lst)) { nb++; lst = CDR(lst); }
    *rest = lst;
    *length = integer_to_object(nb);
}
_ sc_length(sc *sc, _ lst) {
    _ nb;
    _ rest;
    _sc_length_rest(sc, lst, &nb, &rest);
    if (FALSE == sc_is_null(sc, rest)) TYPE_ERROR(lst);
    return nb;
}

//static _ _sc_impure_list_to_vector_and_rest(sc *sc, _* rest) {
//}

// Take n elements from the head of a list and place them in a vector.
_ sc_take_vector(sc *sc, _ n, _ in_lst) {
    _ lst = in_lst;
    long slots = CAST(integer, n);
    _ vo = gc_alloc(sc->gc, slots);
    vector *v = object_to_vector(vo);
    long i;
    for(i=0; i<slots; i++){
        if (FALSE == sc_is_pair(sc, lst)) return TYPE_ERROR(in_lst);
        pair *p = object_to_pair(lst);
        v->slot[i] = p->car;
        lst = p->cdr;
    }
    return vo;
}
_ sc_list_to_vector(sc *sc, _ lst){
    return sc_take_vector(sc, sc_length(sc, lst), lst);
}
_ sc_find_slot(sc *sc, _ E, _ var) {
    if (TRUE == sc_is_null(sc, E)) return FALSE;
    _ slot = CAR(E);
    _ name = CAR(slot);
    if (name == var) return slot;
    else return sc_find_slot(sc, CDR(E), var);
}
_ sc_find(sc *sc, _ E, _ var) {
    _ rv = sc_find_slot(sc, E, var);
    if (FALSE == sc_is_pair(sc, rv)) return FALSE;
    return CDR(rv);
}
_ sc_env_set(sc *sc, _ E, _ var, _ value) {
    _ rv = sc_find_slot(sc, E, var);
    if (FALSE == sc_is_pair(sc, rv)) return FALSE;
    CDR(rv)=value;
    return VOID;
}
_ sc_find_toplevel(sc *sc, _ var) {
    return sc_find(sc, sc->toplevel, var);
}
_ sc_find_toplevel_macro(sc *sc, _ var) {
    return sc_find(sc, sc->toplevel_macro, var);
}
/*  Add to or mutate toplevel. */
_ sc_bang_def_toplevel(sc* sc, _ var, _ val) {
    symbol *s;
    if (!(s=object_to_symbol(var, sc))) TYPE_ERROR(var);
    // printf("DEF %s: ",s->name); sc_post(sc, val);
    if (FALSE == sc_env_set(sc, sc->toplevel, var, val)) {
        sc->toplevel = CONS(CONS(var,val), sc->toplevel);
    }
    return VOID;
}
_ sc_bang_def_toplevel_macro(sc* sc, _ var, _ val) {
    if (!object_to_symbol(var, sc)) TYPE_ERROR(var);
    if (FALSE == sc_env_set(sc, sc->toplevel_macro, var, val)) {
        sc->toplevel_macro = CONS(CONS(var,val), sc->toplevel_macro);
    }
    return VOID;
}
_ sc_toplevel(sc *sc) { return sc->toplevel; }
_ sc_toplevel_macro(sc *sc) { return sc->toplevel_macro; }

_ sc_is_list(sc *sc, _ o) {
    if(TRUE==sc_is_null(sc, o)) return TRUE;
    if(FALSE==sc_is_pair(sc, o)) return FALSE;
    return sc_is_list(sc, CDR(o));
}
_ sc_newline(sc *sc) { printf("\n"); return VOID; }
static _ write_vector(sc *sc, char *type, _ o) {
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
_ sc_write(sc *sc, _ o) {
    if (TRUE  == o) { printf("#t"); return VOID; }
    if (FALSE == o) { printf("#f"); return VOID; }
    if (TRUE == sc_is_integer(sc, o)) {
        printf("%ld", object_to_integer(o));
        return VOID;
    }
    if (VOID  == o) { printf("#<void>"); return VOID; }
    if(TRUE == sc_is_null(sc, o)) {
        printf("()");
        return VOID;
    }
   if (TRUE == sc_is_vector(sc, o))  return write_vector(sc, "", o);
    // FIXME: this doesn't print improper lists other than pairs.
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
    if (TRUE == sc_is_closure(sc, o)) return write_vector(sc, "closure", o);
    if (TRUE == sc_is_state(sc, o))   return write_vector(sc, "state", o);
    if (TRUE == sc_is_lambda(sc, o))  return write_vector(sc, "lambda", o);
    if (TRUE == sc_is_redex(sc, o))   return write_vector(sc, "redex", o);
    if (TRUE == sc_is_error(sc, o))   return write_vector(sc, "error", o);

    if (TRUE == sc_is_k_apply(sc, o)) return write_vector(sc, "k_apply", o);
    if (TRUE == sc_is_k_if(sc, o))    return write_vector(sc, "k_if", o);
    if (TRUE == sc_is_k_seq(sc, o))   return write_vector(sc, "k_seq", o);
    if (TRUE == sc_is_k_set(sc, o))   return write_vector(sc, "k_set", o);
    if (TRUE == sc_is_k_macro(sc, o)) return write_vector(sc, "k_macro", o);
    if (MT   == o) printf("k_mt");

    if (TRUE == sc_is_prim(sc, o)) {
        prim *p = object_to_prim(o,sc);
        printf("#prim<%p:%ld>", (void*)(p->fn),p->nargs);
        return VOID;
    }
    printf("#<%p>",(void*)o);
    return VOID;
}
_ sc_post(sc* sc, _ o) {
    if (VOID != o) {
        sc_write(sc, o);
        printf("\n");
    }
    return VOID;
}

_ sc_fatal(sc *sc) {
    printf("FATAL\n");
    return VOID;
}


/* INTERPRETER */

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

   Other continuations (if, set!, ...) work similarly: they indicate
   what to do next with the recently reduced closure.
*/


static inline _ _sc_call(sc *sc, void *p, int nargs, _ ra) {
    switch(nargs) {
    case 0: return ((sc_0)p)(sc);
    case 1: return ((sc_1)p)(sc, CAR(ra));
    case 2: return ((sc_2)p)(sc, CADR(ra), CAR(ra));
    case 3: return ((sc_3)p)(sc, CADDR(ra), CADR(ra), CAR(ra));
    default:
        return ERROR("prim", integer_to_object(nargs));
    }
}

/* Convert a list of terms obtained as part of an AST of a closure to
   a list of closures. */
_ sc_close_args(sc *sc, _ lst, _ E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(CLOSURE(REDEX(CAR(lst)), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

_ _sc_step_value(sc *sc, _ value, _ k) {

    /* Look at the continuation to determine what to do with the value. 
       
       - empty continuation: halt
       - argument evaluation: eval next, or apply
       - macro expansion result -> interpret the new term.
       - predicate position of an 'if' -> pick yes or no
       - value position of 'set!' -> mutate environment
       - ...
    */

    /* A fully reduced value in an empty continuation means the
       evaluation is finished, and the machine can be halted. */
    if (MT == k) {
        sc_error(sc, SYMBOL("halt"), value);
    }
    if (TRUE == sc_is_k_if(sc, k)) {
        k_if *kx = object_to_k_if(k);
        _ rc = (FALSE == value) ? kx->no : kx->yes;
        return STATE(rc, kx->parent);
    }
    if (TRUE == sc_is_k_set(sc, k)) {
        k_set *kx = object_to_k_set(k);
        closure *v = CAST(closure, kx->var);
        // allocate before mutation
        _ rv = STATE(CLOSURE(VOID,NIL), kx->parent);
        if (FALSE == sc_env_set(sc, v->env, v->term, value)) {
            if (FALSE == sc_env_set(sc, sc->toplevel, v->term, value)) {
                return ERROR("undefined", v->term);
            }
        }
        return rv;
    }
    if (TRUE == sc_is_k_seq(sc, k)) {
        k_seq *kx = object_to_k_seq(k);
        /* There is always at least one next expression. */
        pair *top = CAST(pair, kx->todo);
        /* If this is the last one, replace the continuation, else
           update k_seq. */
        if (NIL == top->cdr) return STATE(top->car, kx->parent);
        return STATE(top->car, sc_make_k_seq(sc, kx->parent, top->cdr));
    }
    if (TRUE == sc_is_k_macro(sc, k)) {
        /* The _ returned by the macro is wrapped as an AST wich
           triggers its further reduction. */
        k_macro *kx = object_to_k_macro(k);
        return STATE(CLOSURE(REDEX(value),kx->env), kx->parent);
    }
    if (TRUE == sc_is_k_apply(sc, k)) {
        /* If there are remaining closures to evaluate, push the value
           to the update value list and pop the next closure. */
        k_apply *kx = object_to_k_apply(k);
        if (TRUE==sc_is_pair(sc, kx->todo)) {
            return STATE(CAR(kx->todo),
                         sc_make_k_apply(sc, kx->parent,
                                         CONS(value, kx->done),
                                         CDR(kx->todo)));
                                          
        }
        /* No more expressions to be reduced in the current k_apply:
           perform application. */
        else {
            _ rev_args = CONS(value, kx->done);
            _ p=rev_args, fn=NIL;
            int n = 0;
            while (TRUE==sc_is_pair(sc,p)) {
                n++; fn = CAR(p); p = CDR(p);
            }
            // n  == 1 + nb_args
            // fn == primitive or lambda

            // unpack the closure
            _ fn_env, fn_term;

            if (TRUE == sc_is_closure(sc, fn)) {
                closure *c = object_to_closure(fn);
                fn_term = c->term;
                fn_env  = c->env;
            }
            else {
                fn_term = fn;
                fn_env  = NIL;
            }

            /* Application of primitive function results in C call. */
            if (TRUE==sc_is_prim(sc, fn_term)) {
                prim *p = object_to_prim(fn_term,sc);
                if (prim_nargs(p) != (n-1)) {
                    return ERROR("nargs", fn_term);
                }
                /* Perform all allocation _before_ the execution of
                   the primitive.  This is to make sure that we won't
                   be the cause of an abort due to GC _after_ the
                   primitive has executed.  Meaning, primitives won't
                   be restarted unless it's their own fault. */
                _ state   = STATE(VOID, kx->parent); // drop frame
                _ rv      = _sc_call(sc, prim_fn(p), n-1, rev_args);
                object_to_state(state)->redex_or_value = rv;
                return state;
            }
            /* Application of abstraction extends the fn_env environment. */
            if (TRUE==sc_is_lambda(sc, fn_term)) {
                lambda *l = CAST(lambda, fn_term);
                vector *v = CAST(vector, l->formals);

                long nb_named_args    = vector_size(v);
                long nb_received_args = n - 1;
                long nb_rest_args     = nb_received_args - nb_named_args;

                if ((nb_rest_args < 0) 
                    || ((NIL == l->rest) &&
                        (nb_rest_args != 0))) return ERROR("nargs", fn_term);

                /* If any, add the rest arguments accumulated in a list. */
                _ rest_args = NIL;
                if (NIL != l->rest) {
                    while (nb_rest_args--) {
                        rest_args = CONS(CAR(rev_args), rest_args);
                        rev_args = CDR(rev_args);
                    }
                    fn_env = CONS(CONS(l->rest, rest_args), fn_env);
                }

                /* Add the named arguments. */
                long i;
                for (i=nb_named_args-1; i>=0; i--) {
                    fn_env = CONS(CONS(v->slot[i], CAR(rev_args)), fn_env);
                    rev_args = CDR(rev_args);
                }
                return STATE(CLOSURE(l->term, fn_env),  // close term
                             kx->parent);               // drop frame
            } 

            /* Continuation */
            if (TRUE==sc_is_k(sc, fn_term)) {
                if (n != 2) ERROR("nargs", fn_term);
                return STATE(CAR(rev_args), fn_term);
            }

            /* Unknown applicant type */
            return ERROR("apply", fn_term);
        }
    }
    /* Unknown continuation type */
    return ERROR("cont", k);
}


static _ _sc_step(sc *sc, _ o_state) {

    /* The state consists of a closure (a possibly reducable, possibly
       open term and its environment) and a continuation (a data
       structure that encodes what to do with a fully reduced value).

       The machine tries to either reduce the current closure, or
       update the current continuation with the current value (=
       non-reducable closure). */

    _ term, env, k;  // C E K
    {
        state *s = CAST(state, o_state);
        k = s->continuation;

        /* Determine term and environment.
           
           The environment and state can contain naked values with an
           implied empty envionment.  This representation makes C
           primitives simpler.  */

        if ((TRUE == sc_is_closure(sc, s->redex_or_value))) {
            closure *c = object_to_closure(s->redex_or_value);
            term = c->term;
            env  = c->env;
        }
        else {
            term = s->redex_or_value;
            env  = NIL;
        }

        /* Fully reduced expression: strip environment if it is no
           longer needed and pass it to the current continuation. */
        if (FALSE==sc_is_redex(sc, term)) {
            _ value = (TRUE==sc_is_lambda(sc, term)) 
                ? s->redex_or_value : term;
            return _sc_step_value(sc, value, k);
        }
        
        /* Reducable: unpack s-expression wrapper. */
        term = object_to_redex(term)->datum;
    }



    /* Abstract Syntax: perform a single reduction step.

       - create abstraction (lambda) value
       - create application continuation
       - perform variable reference
       - create special form contiuation (if, set!, macro, ...)
     */

    /* Variable Reference */
    if (TRUE==sc_is_symbol(sc, term)){
        _ val; 
        if (FALSE == (val = sc_find(sc, env, term))) {
            if (FALSE == (val = sc_find_toplevel(sc, term))) {
                return ERROR("undefined", term);
            }
        }
        return STATE(val, k); // wrap naked values
    }

    /* Literal Value */
    if (FALSE==sc_is_pair(sc, term)) {
        return STATE(CLOSURE(term,env), k);
    }

    _ term_f    = CAR(term);
    _ term_args = CDR(term);

    /* Special Form */
    if (TRUE==sc_is_symbol(sc, term_f)) {
        if (term_f == sc->s_lambda) {
            if (NIL == term_args) ERROR("syntax",term);
            _ argspec = CAR(term_args);
            _ named;
            _ rest;
            _sc_length_rest(sc, argspec, &named, &rest);
            if ((NIL   != rest) &&
                (FALSE == sc_is_symbol(sc,rest))) {
                ERROR("syntax",term);
            }
            _ formals = sc_take_vector(sc, named, argspec);
            /* Implement the expression sequence in a `lambda'
               expression as a `begin' sequencing form. */
            _ body = CDR(term_args);
            if (NIL == CDR(body)) body = CAR(body);
            else body = CONS(sc->s_begin, body);
            _ l = sc_make_lambda(sc, formals, rest, REDEX(body));
            return STATE(CLOSURE(l, env), k);
        }
        if (term_f == sc->s_quote) {
            if (NIL == term_args) ERROR("syntax",term);
            return STATE(CLOSURE(CAR(term_args),env), k);
        }
        if (term_f == sc->s_if) {
            if (NIL == term_args) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ cond = CLOSURE(REDEX(CAR(term_args)),env);
            _ yes  = CLOSURE(REDEX(CADR(term_args)),env);
            _ no   = 
                (NIL == CDDR(term_args)) ? 
                CLOSURE(VOID,NIL) :
                CLOSURE(REDEX(CADDR(term_args)),env);
            return STATE(cond, sc_make_k_if(sc, k, yes,no));
                                              
        }
        if (term_f == sc->s_bang_set) {
            if (NIL == term_args) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ var = CLOSURE(CAR(term_args),env);
            _ cl  = CLOSURE(REDEX(CADR(term_args)),env);
            return STATE(cl, sc_make_k_set(sc, k, var));
        }
        if (term_f == sc->s_begin) {
            if (FALSE == sc_is_pair(sc, term_args)) ERROR("syntax",term);
            _ todo = sc_close_args(sc, term_args, env);
            pair *body = object_to_pair(todo);
            /* Don't create a contination if there's only a single
               expression.*/
            if (NIL == body->cdr) return STATE(body->car, k);
            return STATE(body->car, sc_make_k_seq(sc, k, body->cdr));
        }                
        if (term_f == sc->s_letcc) {
            if (NIL == term_args) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ var = CAR(term_args);
            env   = CONS(CONS(var,k),env);
            _ cl  = CLOSURE(REDEX(CADR(term_args)),env);
            return STATE(cl, k);
        }
        _ macro;
        if (FALSE != (macro = sc_find(sc, sc->toplevel_macro, term_f))) {
            /* Macro continuation is based on a completed
               k_apply frame that will trigger the fn
               application, linked to a k_macro frame that
               will steer the result back to the AST
               reducer. */
            _ k_m = sc_make_k_macro(sc, k, env);
            _ k_a = sc_make_k_apply
                (sc, k_m,
                 CONS(macro, NIL), // done list
                 NIL);             // todo list
            return STATE(CLOSURE(term,NIL), k_a);
        }

        /* Fallthrough: symbol must be bound to applicable values. */
    }

    /* Application */

    /* Extend the continuation with a new frame by collecting
       all (open) subterms, and binding them to the current
       environment. */
    _ closed_args = sc_close_args(sc, term_args, env);
    return STATE(CLOSURE(REDEX(term_f), env),
                 sc_make_k_apply(sc, k, NIL, closed_args));
}



/* Because interpreter-step is accessible from Scheme, it is possible
   to create towers of interpreters.  This requires some nesting for
   the exceptions, which only travel towards the nearest step() to be
   turned into values for the layer above.

   Note that GC exceptions are different: they travel all the way up
   to the topmost STEP and restart its continuation.  

   Only GC and the topmost STEP are allowed to access sc->state.
*/

_ sc_eval_step(sc *sc, _ state) {
    int exception;
    object rv = NIL;
    jmp_buf save;
    memcpy(&save, &sc->step, sizeof(save));
    sc->entries++;

    switch(exception = setjmp(sc->step)) {
        case SC_EX_TRY:
            rv = _sc_step(sc, state);
            break;
        case SC_EX_ABORT: 
            rv = sc_make_error(sc, sc->error_tag, sc->error_arg, state);
            sc->error_arg = NIL;
            sc->error_tag = NIL;
            break;
        default:
            break;
    }
    memcpy(&sc->step, &save, sizeof(save));
    sc->entries--;
    return rv;
}





/* While sc_eval_step() is a pure function, some primitives
   require direct modification of the continuation.  We do this using
   assignment of sc->state and the SC_EX_RESTART exception. */

static _ _sc_prim_k(sc *sc) {
    state *s = CAST(state, sc->state);
    /* We know we're in a k_apply continuation because we can only end
       up here through primitive execution.  FIXME: It's probably
       better to put the parent field in the same location for all
       k_xxx structs. */
    k_apply *f = CAST(k_apply, s->continuation);
    return f->parent;
}
static _ _sc_restart(sc *sc) { longjmp(sc->run, SC_EX_RESTART); }
   
/* GC: set continuation manually, since since the interpreter aborts
   and restarts the current step. */
_ sc_gc(sc* sc) {
    sc->state = STATE(CLOSURE(VOID,NIL), _sc_prim_k(sc));
    gc_collect(sc->gc);
    return NIL;
}

/* Continuation transformer for apply.  This uses k_ignore to pass a
   value to a k_apply continuation.  (It would be simpler if k_apply
   evaluated from right to left, so the awkwardness here is due to
   implementation: I need evaluation from left to right.) */
_ sc_apply_ktx(sc* sc, _ k, _ fn, _ args) {
    object done;
    object value;
    if (NIL == args) {
        done = NIL;
        value = fn;
    }
    else {
        done = CONS(fn, NIL);
        pair *p;
        while ((p = CAST(pair,args)) && (NIL != p->cdr)) {
            done = CONS(p->car, done);
            args = p->cdr;
        }
        value = p->car;
    }
    object app = sc_make_k_apply(sc, k, done, NIL); // all but last
    object seq = sc_make_k_seq(sc, app, CONS(value, NIL));
    return seq;
}





/* --- SETUP & GC --- */

/* Toplevel eval.  This function captures the GC restart.

     - This function is NOT re-entrant.  The primitive
       sc_eval_step() however can be called recursively.

     - It is allowed to use gc_alloc() outside this loop to create
       data (to pass to this function) as long as you can prove that
       there will be no collection.  Triggering GC outside of this
       function will invalidate previously allocated data (it will
       have moved).
*/

_ _sc_eval(sc *sc, _ expr){
    sc->state = STATE(expr,MT);
    int exception;
    if (sc->entries) return NIL;
    sc->entries++;
  next:
    switch (exception = setjmp(sc->run)) {
    case SC_EX_TRY:
        do {
            // sc_post(sc, sc->state);
            sc->state = sc_eval_step(sc, sc->state); 
        } while (FALSE == sc_is_error(sc, sc->state));
        /* Handle error states */
        error *e = object_to_error(sc->state);
        if (e->tag == SYMBOL("halt")) {
            expr = e->arg;
            goto leave;
        }
        else {
            // FIXME: stay inside the interpreter.
            printf("ERROR: ");
            sc_write(sc, e->tag); printf(": ");
            sc_write(sc, e->arg); printf("\n");
            goto abort;
        }
    case SC_EX_RESTART: goto next;
    default:
        fprintf(stderr, "Unknown exception %d.\n", exception);
        goto abort;
    }
  leave:
    sc->entries--;
    return expr;
  abort:
    sc->state = sc->state_abort;
    goto next;
}

#define MARK(field) sc->field = gc_mark(sc->gc, sc->field)
static void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // sc_trap(sc);
    // printf("GC mark()\n");
    // sc_post(sc, sc->state);
    MARK(state);
    MARK(state_abort);
    MARK(toplevel);
    MARK(toplevel_macro);
    fin(sc->gc);
    // sc_post(sc, sc->state);
    /* Abort C stack, since it now contains invalid refs.

       Note: This prevents the GC to grow the heap size, which means
             it becomes _our_ responsability to ensure the restarting
             doesn't turn into an infinite loop (when the current
             evaluation step won't make it to the next before
             triggering collection).
    */
    if (sc->entries) _sc_restart(sc);
    printf("WARNING: triggering GC outside of the main loop.\n");
}
static _ _sc_make_prim(sc *sc, void *fn, long nargs) {
    prim *p = malloc(sizeof(*p));
    p->a.op = &sc->op_prim; // class
    p->fn = fn;
    p->nargs = nargs;
    return atom_to_object(&p->a);
}
void _sc_def_prim(sc *sc, _ var, void *fn, long nargs) {
    sc_bang_def_toplevel(sc, var, _sc_make_prim(sc, fn, nargs));
}
sc *_sc_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->entries = 0;

    /* Garbage collector. */
    sc->gc = gc_new(1000000, (gc_mark_roots)_sc_mark_roots, sc);

    /* Atom classes. */
    sc->syms = symstore_new(1000);
    sc->op_prim.free = NULL;

    /* Environments */
    sc->toplevel       = NIL;
    sc->toplevel_macro = NIL;

    /* Toplevel continuation */
    _ abort = REDEX(CONS(SYMBOL("fatal"),NIL));
    sc->state_abort = STATE(abort,MT);

    /* Cached identifiers */
    sc->s_lambda   = SYMBOL("lambda");
    sc->s_if       = SYMBOL("if");
    sc->s_bang_set = SYMBOL("set!");
    sc->s_quote    = SYMBOL("quote");
    sc->s_begin    = SYMBOL("begin");
    sc->s_letcc    = SYMBOL("letcc");

    /* Primitive defs */
    _sc_def_prims(sc); // defined in scheme.h_

    /* Highlevel bootstrap */
#include "boot.c_"
    return sc;
}



