#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "symbol.h"
#include "scheme.h"

// generated
#include "scheme.h_"
#include "boot.h_"

/* --- PRIMITIVES --- */

/* To simplify the implementation, as much as possible functions are
   implemented as Scheme primitives operating on Scheme values.  They
   use the prefix "sc_".

   The functions operating on *sc that are too lowlevel to respect the
   "sc_" ABI (because they use values that cannot be represented as a
   Scheme object, or because they violate behavioural constraints) are
   prefixed "_sc".  These are kept to a minimum to avoid duplication
   and make most of the functionality available to scheme.

   Note in particular that "sc_eval_step()" is re-entrant, and that
   interpreter data constructors are available in Scheme to construct
   modified interpreters.

*/

static _ _sc_assert(sc *sc, sc_1 predicate, _ o) {
    if (FALSE == predicate(sc, o)) { TYPE_ERROR(o); }
    return o;
}

/* Booleans are GC_CONST */
_ sc_is_bool(sc *sc, _ o) {
    void *x;
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}
_ sc_is_integer(sc *sc, _ o) {
    if (GC_INTEGER == GC_TAG(o)) return TRUE;
    return FALSE;
}
_ sc_is_zero(sc *sc, _ o) {
    long i = CAST(integer, o);
    if (i) return FALSE;
    return TRUE;
}
_ sc_add1(sc *sc, _ o) {
    long i = CAST(integer, o);
    return integer_to_object(i + 1);
}
_ sc_sub1(sc *sc, _ o) {
    long i = CAST(integer, o);
    return integer_to_object(i - 1);
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
#define TAG_VALUE     4
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
_ sc_is_state(sc *sc, _ o)   { return vector_type(o, TAG_STATE); }
_ sc_is_redex(sc *sc, _ o)   { return vector_type(o, TAG_REDEX); }
_ sc_is_value(sc *sc, _ o)   { return vector_type(o, TAG_VALUE); }
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
// E = environment (Et = toplevel)
// P = parent continuation
// D = datum



_ sc_cons(sc *sc, _ car, _ cdr)              {STRUCT(TAG_PAIR,    2, car,cdr);}
_ sc_make_state(sc *sc, _ C, _ K)            {STRUCT(TAG_STATE,   2, C,K);}
_ sc_make_lambda(sc *sc, _ F, _ R, _ S, _ E) {STRUCT(TAG_LAMBDA , 4, F,R,S,E);}
_ sc_make_error(sc *sc, _ T, _ A, _ K, _ X)  {STRUCT(TAG_ERROR,   4, T,A,K,X);}
_ sc_make_redex(sc *sc, _ D, _ E)            {STRUCT(TAG_REDEX,   2, D,E);}
_ sc_make_value(sc *sc, _ D)                 {STRUCT(TAG_VALUE,   1, D);}


// 'P' is in slot 0
// continuations are created with an empty mark list
_ sc_make_k_apply(sc *sc, _ P, _ D, _ T)     {STRUCT(TAG_K_APPLY,  4, P,NIL,D,T);}
_ sc_make_k_if(sc *sc, _ P, _ Y, _ N)        {STRUCT(TAG_K_IF,     4, P,NIL,Y,N);}
_ sc_make_k_set(sc *sc, _ P, _ V, _ E, _ Et) {STRUCT(TAG_K_SET,    5, P,NIL,V,E,Et);}
_ sc_make_k_seq(sc *sc, _ P, _ T)            {STRUCT(TAG_K_SEQ,    3, P,NIL,T);}
_ sc_make_k_macro(sc *sc, _ P, _ E)          {STRUCT(TAG_K_MACRO,  3, P,NIL,E);}


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
    if (sc->step_entries) longjmp(sc->r.step, SC_EX_ABORT);
    fprintf(stderr, "ERROR: attempt to abort primitive outside of the main loop.\n");
    sc_trap(sc);
    exit(1);
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
    if (FALSE == sc_is_null(sc, rest)) {
        TYPE_ERROR(lst);
    }
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
static _ *vector_index(sc *sc, _ vec, _ n) {
    vector *v = CAST(vector, vec);
    long index = CAST(integer, n);
    if ((index < 0) || (index >= vector_size(v))) ERROR("ref", n);
    return &v->slot[index];
}
_ sc_vector_ref(sc *sc, _ vec, _ n) {
    return *vector_index(sc, vec, n);
}
_ sc_bang_vector_set(sc *sc, _ vec, _ n, _ val) {
    *vector_index(sc, vec, n) = val;
    return VOID;
}


_ sc_global(sc *sc, _ n) { 
    return sc_vector_ref(sc, sc->global, n); 
}
_ sc_bang_set_global(sc *sc, _ n, _ val) { 
    return sc_bang_vector_set(sc, sc->global, n, val); 
}

#define GLOBAL(name) return sc_global(sc, sc_slot_##name)
#define GLOBAL_SET(name, val) return sc_bang_set_global(sc, sc_slot_##name, val)

_ sc_toplevel(sc *sc)       { GLOBAL(toplevel); }
_ sc_toplevel_macro(sc *sc) { GLOBAL(toplevel_macro); }
_ sc_state(sc *sc)          { GLOBAL(state); }
_ sc_abort_k(sc *sc)        { GLOBAL(abort_k); }

_ sc_bang_set_toplevel(sc *sc, _ val)       { GLOBAL_SET(toplevel, val); }
_ sc_bang_set_toplevel_macro(sc *sc, _ val) { GLOBAL_SET(toplevel_macro, val); }

_ sc_find_toplevel(sc *sc, _ var) {
    return sc_find(sc, sc_toplevel(sc), var);
}
_ sc_find_toplevel_macro(sc *sc, _ var) {
    return sc_find(sc, sc_toplevel_macro(sc), var);
}
/*  Add to or mutate toplevel env. */
_ sc_bang_def_global(sc* sc, _ slot, _ var, _ val) {
    symbol *s;
    _ env = sc_global(sc, slot);
    if (!(s=object_to_symbol(var, sc))) TYPE_ERROR(var);
    // printf("DEF %s: ",s->name); sc_post(sc, val);
    if (FALSE == sc_env_set(sc, env, var, val)) {
        sc_bang_set_global(sc, slot, CONS(CONS(var,val), env));
    }
    return VOID;
}
_ sc_bang_def_toplevel(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel, var, val);
}
_ sc_bang_def_toplevel_macro(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel_macro, var, val);
}
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
    if (TRUE == sc_is_state(sc, o))   return write_vector(sc, "state", o);
    if (TRUE == sc_is_lambda(sc, o))  return write_vector(sc, "lambda", o);
    if (TRUE == sc_is_redex(sc, o))   return write_vector(sc, "redex", o);
    if (TRUE == sc_is_value(sc, o))   return write_vector(sc, "value", o);
    if (TRUE == sc_is_error(sc, o))   return write_vector(sc, "error", o);

    if (TRUE == sc_is_k_apply(sc, o)) return write_vector(sc, "k_apply", o);
    if (TRUE == sc_is_k_if(sc, o))    return write_vector(sc, "k_if", o);
    if (TRUE == sc_is_k_seq(sc, o))   return write_vector(sc, "k_seq", o);
    if (TRUE == sc_is_k_set(sc, o))   return write_vector(sc, "k_set", o);
    if (TRUE == sc_is_k_macro(sc, o)) return write_vector(sc, "k_macro", o);
    if (MT   == o) { printf("#k_mt"); return VOID; }

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
_ sc_read_char(sc *sc) {
    return integer_to_object(fgetc(stdin));
}
_ sc_is_eq(sc *sc, _ a, _ b) {
    if (a == b) return TRUE;
    return FALSE;
}


_ sc_fatal(sc *sc, _ err) {
    if (TRUE == sc_is_error(sc, err)) {
        error *e = object_to_error(err);
        printf("ERROR");
        if (TRUE == sc_is_prim(sc, e->prim)) {
            prim *p = object_to_prim(e->prim, sc);
            symbol *s = object_to_symbol(p->var, sc);
            if (s) printf(" in `%s'", s->name); 
        }
        printf(": ");
        sc_write(sc, e->tag); printf(": ");
        sc_write(sc, e->arg); printf("\n");
    }
    return VOID;
}

_ sc_mt(sc *sc)    { return MT; }
_ sc_true(sc *sc)  { return TRUE; }
_ sc_false(sc *sc) { return FALSE; }
_ sc_void(sc *sc)  { return VOID; }

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

/* Propagate environment during reduction. */
_ sc_close_args(sc *sc, _ lst, _ E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(REDEX(CAR(lst), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

_ _sc_step_value(sc *sc, _ v, _ k) {

    /* Look at the continuation to determine what to do with the value. 
       
       - empty continuation: halt
       - argument evaluation: eval next, or apply
       - macro expansion result -> interpret the new term.
       - predicate position of an 'if' -> pick yes or no
       - value position of 'set!' -> mutate environment
       - ...
    */

    /* Unwrap */
    value *vx = CAST(value, v);
    _ value = vx->datum;

    /* A fully reduced value in an empty continuation means the
       evaluation is finished, and the machine can be halted. */
    if (MT == k) {
        sc_error(sc, SYMBOL("halt"), value);
    }
    if (TRUE == sc_is_k_if(sc, k)) {
        k_if *kx = object_to_k_if(k);
        _ rc = (FALSE == value) ? kx->no : kx->yes;
        return STATE(rc, kx->k.parent);
    }
    if (TRUE == sc_is_k_set(sc, k)) {
        k_set *kx = object_to_k_set(k);
        // allocate before mutation
        _ rv = STATE(VALUE(VOID), kx->k.parent);
        if (FALSE == sc_env_set(sc, kx->env, kx->var, value)) {
            if (FALSE == sc_env_set(sc, 
                                    sc_global(sc, kx->tl_slot),  // global toplevel
                                    kx->var, value)) {
                return ERROR("undefined", kx->var);
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
        if (NIL == top->cdr) return STATE(top->car, kx->k.parent);
        return STATE(top->car, sc_make_k_seq(sc, kx->k.parent, top->cdr));
    }
    if (TRUE == sc_is_k_macro(sc, k)) {
        /* The _ returned by the macro is wrapped as an AST wich
           triggers its further reduction. */
        k_macro *kx = object_to_k_macro(k);
        return STATE(REDEX(value,kx->env), kx->k.parent);
    }
    if (TRUE == sc_is_k_apply(sc, k)) {
        /* If there are remaining closures to evaluate, push the value
           to the update value list and pop the next closure. */
        k_apply *kx = object_to_k_apply(k);
        if (TRUE==sc_is_pair(sc, kx->todo)) {
            return STATE(CAR(kx->todo),
                         sc_make_k_apply(sc, kx->k.parent,
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
            // fn == primitive, lambda or continuation

            /* Application of primitive function results in C call. */
            if (TRUE==sc_is_prim(sc, fn)) {
                prim *p = object_to_prim(fn,sc);
                sc->r.prim = fn; // for debug
                if (prim_nargs(p) != (n-1)) {
                    return ERROR("nargs", fn);
                }
                /* Perform all allocation _before_ the execution of
                   the primitive.  This is to make sure that we won't
                   be the cause of an abort due to GC _after_ the
                   primitive has executed.  Meaning, a primitive won't
                   be restarted if it doesn't call gc_alloc(). */
                _ value   = VALUE(VOID);
                _ state   = STATE(value, kx->k.parent); // drop frame
                _ rv      = _sc_call(sc, prim_fn(p), n-1, rev_args);
                object_to_value(value)->datum = rv;
                    
                return state;
            }
            /* Application of abstraction extends the fn_env environment. */
            if (TRUE==sc_is_lambda(sc, fn)) {
                lambda *l = CAST(lambda, fn);
                vector *v = CAST(vector, l->formals);
                _ fn_env = l->env;

                long nb_named_args    = vector_size(v);
                long nb_received_args = n - 1;
                long nb_rest_args     = nb_received_args - nb_named_args;

                if ((nb_rest_args < 0) 
                    || ((NIL == l->rest) &&
                        (nb_rest_args != 0))) return ERROR("nargs", fn);

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
                return STATE(REDEX(l->term, fn_env),  // close term
                             kx->k.parent);           // drop frame
            } 

            /* Continuation */
            if (TRUE==sc_is_k(sc, fn)) {
                _ arg = VOID; // no args to k -> inserts void.
                if (n > 2) ERROR("nargs", fn);
                if (n == 2) arg = CAR(rev_args);
                return STATE(VALUE(arg), fn);
            }

            /* Unknown applicant type */
            return ERROR("apply", fn);
        }
    }
    /* Unknown continuation type */
    return ERROR("cont", k);
}


static _ _sc_step(sc *sc, _ o_state) {

    /* The state consists of:

       - a closure: a possibly reducible (redex or value), open term
                    and its environment
       
       - a continuation: a data structure that encodes what to do with
                         a fully reduced value.

       The machine tries to either reduce the redex, or update the
       current continuation with the current value (= non-reducible
       closure). */

    _ term, env, k;  // C E K

    state *s = CAST(state, o_state);
    k = s->continuation;

    /* Values */
    if (FALSE==sc_is_redex(sc, s->redex_or_value)) {
        return _sc_step_value(sc, s->redex_or_value, k);
    }
    /* Determine term and environment: The redex can contain naked
       values with an implied empty envionment. */
    else {
        redex *r = CAST(redex, s->redex_or_value);
        env  = r->env;
        term = r->term;
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
        return STATE(VALUE(val), k);
    }

    /* Literal Value */
    if (FALSE==sc_is_pair(sc, term)) {
        _ val = (TRUE==sc_is_lambda(sc, term)) 
            ? s->redex_or_value : term;

        return STATE(VALUE(val),k);
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
            _ l = sc_make_lambda(sc, formals, rest, body, env);
            return STATE(VALUE(l), k);
        }
        if (term_f == sc->s_quote) {
            if (NIL == term_args) ERROR("syntax",term);
            return STATE(VALUE(CAR(term_args)), k);
        }
        if (term_f == sc->s_if) {
            if (NIL == term_args) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ cond = REDEX(CAR(term_args),env);
            _ yes  = REDEX(CADR(term_args),env);
            _ no   = 
                (NIL == CDDR(term_args)) ? 
                VALUE(VOID) :
                REDEX(CADDR(term_args),env);
            return STATE(cond, sc_make_k_if(sc, k, yes,no));
                                              
        }
        if (term_f == sc->s_bang_set) {
            if (NIL == term_args) ERROR("syntax",term);
            _ var = CAR(term_args);
            if (FALSE == sc_is_symbol(sc, var)) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ expr = CADR(term_args);
            return STATE(REDEX(expr, env),
                         sc_make_k_set(sc, k, var, env, sc_slot_toplevel));
        }
        if (term_f == sc->s_begin) {
            if (FALSE == sc_is_pair(sc, term_args)) ERROR("syntax",term);
            _ todo = sc_close_args(sc, term_args, env);
            pair *body = object_to_pair(todo);
            /* Don't create a contination frame if there's only a
               single expression. */
            if (NIL == body->cdr) return STATE(body->car, k);
            return STATE(body->car, sc_make_k_seq(sc, k, body->cdr));
        }                
        if (term_f == sc->s_letcc) {
            if (NIL == term_args) ERROR("syntax",term);
            if (NIL == CDR(term_args)) ERROR("syntax",term);
            _ var = CAR(term_args);
            env   = CONS(CONS(var,k),env);
            _ cl  = REDEX(CADR(term_args),env);
            return STATE(cl, k);
        }
        _ macro;
        if (FALSE != (macro = sc_find(sc, sc_toplevel_macro(sc), term_f))) {
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
            return STATE(VALUE(term), k_a);
        }

        /* Fallthrough: symbol must be bound to applicable values. */
    }

    /* Application */

    /* Extend the continuation with a new frame by collecting
       all (open) subterms, and binding them to the current
       environment. */
    _ closed_args = sc_close_args(sc, term_args, env);
    return STATE(REDEX(term_f, env),
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
    scheme_r save;

    memcpy(&save, &sc->r, sizeof(save));
    sc->r.prim = FALSE; // means error comes from step() itself
    sc->step_entries++;

    switch(exception = setjmp(sc->r.step)) {
        case SC_EX_TRY:
            rv = _sc_step(sc, state);
            break;
        case SC_EX_ABORT: 
            rv = sc_make_error(sc, sc->error_tag, sc->error_arg, 
                               state, sc->r.prim);
            sc->error_arg = NIL;
            sc->error_tag = NIL;
            break;
        default:
            break;
    }

    memcpy(&sc->r, &save, sizeof(save));
    sc->step_entries--;
    return rv;
}





static _ _sc_restart(sc *sc) {
    if (sc->top_entries) {
        longjmp(sc->top, SC_EX_RESTART); 
    }
    fprintf(stderr, "ERROR: attempt restart outside of the main loop.\n");
    sc_trap(sc);
    exit(1);
}
   
/* GC: set continuation manually, since since the interpreter aborts
   and restarts the current step. */
_ sc_gc(sc* sc) {
    state *s = CAST(state, sc_global(sc, sc_slot_state));
    _ k = sc_k_parent(sc, s->continuation); // drop `gc' k_apply frame
    sc_bang_set_global(sc, sc_slot_state, 
                       STATE(VALUE(VOID), k)); // update state manually
    gc_collect(sc->gc);                        // collect will restart at sc->state
    return NIL; // not reached
}

_ sc_gc_used(sc *sc) {
    return integer_to_object(sc->gc->current_index);
}

/* Continuation transformer for apply.  

   This uses k_seq to ignore the value passed to the continuation, and
   pass a value to a k_apply continuation.  It would be simpler if
   k_apply evaluated from right to left (which I don't want), so the
   awkwardness here is due to implementation. */
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
        value = VALUE(p->car);
    }
    object app = sc_make_k_apply(sc, k, done, NIL); // all but last
    object seq = sc_make_k_seq(sc, app, CONS(value, NIL));
    return seq;
}

_ sc_eval_ktx(sc *sc, _ k, _ expr) {
    return sc_make_k_seq(sc, k, CONS(REDEX(expr, NIL),NIL));
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

_ _sc_top(sc *sc, _ expr){
    if (sc->top_entries) {
        printf("WARNING: multiple _sc_top() entries.\n");
        return NIL;
    }
    sc->top_entries++;
    sc_bang_set_global(sc, sc_slot_state, STATE(REDEX(expr,NIL),MT));
    for(;;) {
        if (setjmp(sc->top)){
            sc->step_entries = 0;  // full tower unwind
            sc->r.prim = FALSE;
        }
        for(;;) {
            _ state;
            /* Run */
            do {
                state = sc_global(sc, sc_slot_state);         // get
                state = sc_eval_step(sc, state);              // update (functional)
                sc_bang_set_global(sc, sc_slot_state, state); // set
            }
            while (FALSE == sc_is_error(sc, state));
            
            /* Halt */
            error *e = object_to_error(state);
            if (e->tag == SYMBOL("halt")) {
                sc->top_entries--;
                return e->arg;
            }
            
            /* Abort */
            sc_bang_set_global(sc, sc_slot_state,
                               STATE(VALUE(state), 
                                     sc_global(sc, sc_slot_abort_k)));
        }
    }
}

static void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // sc_trap(sc);
    printf("GC mark()\n");
    // sc_post(sc, sc->state);
    sc->global = gc_mark(sc->gc, sc->global);
    fin(sc->gc);
    // sc_post(sc, sc->state);
    /* Abort C stack, since it now contains invalid refs.

       Note: This prevents the GC to grow the heap size, which means
             it becomes _our_ responsability to ensure the restarting
             doesn't turn into an infinite loop (when the current
             evaluation step won't make it to the next before
             triggering collection).
    */
    _sc_restart(sc);
}
static _ _sc_make_prim(sc *sc, void *fn, long nargs, _ var) {
    prim *p = malloc(sizeof(*p));
    p->a.op = &sc->op_prim; // class
    p->fn = fn;
    p->nargs = nargs;
    p->var = var;
    return atom_to_object(&p->a);
}
void _sc_def_prim(sc *sc, _ var, void *fn, long nargs) {
    sc_bang_def_toplevel(sc, var, _sc_make_prim(sc, fn, nargs, var));
}
sc *_sc_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->top_entries = 0;
    sc->step_entries = 0;

    /* Garbage collector. */
    sc->gc = gc_new(10000, (gc_mark_roots)_sc_mark_roots, sc);

    /* Atom classes. */
    sc->syms = symstore_new(1000);
    sc->op_prim.free = NULL;

    sc->global = gc_vector(sc->gc, 4,
                           NIL,  // toplevel
                           NIL,  // macro
                           NIL,  // state
                           NIL); // abort

    /* Cached identifiers */
    sc->s_lambda   = SYMBOL("lambda");
    sc->s_if       = SYMBOL("if");
    sc->s_bang_set = SYMBOL("set!");
    sc->s_quote    = SYMBOL("quote");
    sc->s_begin    = SYMBOL("begin");
    sc->s_letcc    = SYMBOL("letcc");

    /* Primitive defs */
    _sc_def_prims(sc); // defined in scheme.h_

    /* Toplevel abort continuation */
    _ done = CONS(sc_find_toplevel(sc, SYMBOL("fatal")),NIL);
    _ abort_k = sc_make_k_apply(sc, MT, done, NIL);
    sc_bang_set_global(sc, sc_slot_abort_k, abort_k);

    /* Highlevel bootstrap */
    _load(sc);  // from boot.h_
    return sc;
}



