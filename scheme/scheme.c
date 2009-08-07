#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

#include "symbol.h"
#include "scheme.h"



/* --- PRIMITIVES --- */

/* To simplify the implementation _all_ functions are Scheme
   primitives operating on tagged values.  They are named sc_xxx for
   ease of lifting them from the source file for environment
   bootstrap.

   The _sc_xxx helper functions operate on sc*, but do not have
   primitive ABI.

*/

static object _sc_assert(sc *sc, sc_1 predicate, object o) {
    if (FALSE == predicate(sc, o)) { TYPE_ERROR(o); }
    return o;
}

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
        (TAG_VECTOR == vector_get_tag(o))) { return TRUE; }
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
object sc_is_ast(sc *sc, object o)     { return vector_type(o, TAG_AST); }
object sc_is_k_if(sc *sc, object o)    { return vector_type(o, TAG_K_IF); }
object sc_is_k_apply(sc *sc, object o) { return vector_type(o, TAG_K_APPLY); }
object sc_is_k_seq(sc *sc, object o)   { return vector_type(o, TAG_K_SEQ); }
object sc_is_k_set(sc *sc, object o)   { return vector_type(o, TAG_K_SET); }
object sc_is_k_macro(sc *sc, object o) { return vector_type(o, TAG_K_MACRO); }



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
object sc_make_k_apply(sc *sc, object done, object todo, object parent) {
    object o = gc_vector(sc->gc, 3, done, todo, parent);
    vector_set_tag(o, TAG_K_APPLY);
    return o;
}
object sc_make_k_if(sc *sc, object yes, object no, object parent){
    object o = gc_vector(sc->gc, 3, yes, no, parent);
    vector_set_tag(o, TAG_K_IF);
    return o;
}
object sc_make_k_set(sc *sc, object var, object parent) {
    object o = gc_vector(sc->gc, 2, var, parent);
    vector_set_tag(o, TAG_K_SET);
    return o;
}
object sc_make_k_seq(sc *sc, object todo, object parent) {
    object o = gc_vector(sc->gc, 2, todo, parent);
    vector_set_tag(o, TAG_K_SEQ);
    return o;
}
object sc_make_k_macro(sc *sc, object env, object parent){
    object o = gc_vector(sc->gc, 2, env, parent);
    vector_set_tag(o, TAG_K_MACRO);
    return o;
}
object sc_make_ast(sc *sc, object datum){
    object o = gc_vector(sc->gc, 1, datum);
    vector_set_tag(o, TAG_AST);
    return o;
}
object sc_car(sc *sc, object o) { pair *p = CAST(pair, o); return p->car; }
object sc_cdr(sc *sc, object o) { pair *p = CAST(pair, o); return p->cdr; }

/* Error handling:
   FIXME: The machine step() is protected with setjmp(). */
object sc_trap(sc *sc) {
    kill(getpid(), SIGTRAP);
    return VOID;
}
object sc_error(sc *sc, object sym_o, object o) {
    symbol *sym   = object_to_symbol(sym_o, sc);
    if (!sym) sym = string_to_symbol(sc->syms, "error");
    printf("ERROR: ");
    sc_write(sc, sym_o); printf(": ");
    sc_write(sc, o);     printf("\n");
    sc_trap(sc);
    longjmp(sc->step, SC_EX_ABORT);
    return NIL;
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
object sc_find_slot(sc *sc, object E, object var) {
    if (TRUE == sc_is_null(sc, E)) return FALSE;
    object slot = CAR(E);
    object name = CAR(slot);
    if (name == var) return slot;
    else return sc_find_slot(sc, CDR(E), var);
}
object sc_find(sc *sc, object E, object var) {
    object rv = sc_find_slot(sc, E, var);
    if (FALSE == sc_is_pair(sc, rv)) return FALSE;
    return CDR(rv);
}
object sc_env_set(sc *sc, object E, object var, object value) {
    object rv = sc_find_slot(sc, E, var);
    if (FALSE == sc_is_pair(sc, rv)) return FALSE;
    CDR(rv)=value;
    return VOID;
}
object sc_find_toplevel(sc *sc, object var) {
    return sc_find(sc, sc->toplevel, var);
}
object sc_is_list(sc *sc, object o) {
    if(TRUE==sc_is_null(sc, o)) return TRUE;
    if(FALSE==sc_is_pair(sc, o)) return FALSE;
    return sc_is_list(sc, CDR(o));
}
object sc_newline(sc *sc) { printf("\n"); return VOID; }
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
    if (TRUE == sc_is_ast(sc, o))     return write_vector(sc, "ast", o);

    if (TRUE == sc_is_k_apply(sc, o)) return write_vector(sc, "k_apply", o);
    if (TRUE == sc_is_k_if(sc, o))    return write_vector(sc, "k_if", o);
    if (TRUE == sc_is_k_seq(sc, o))   return write_vector(sc, "k_seq", o);
    if (TRUE == sc_is_k_set(sc, o))   return write_vector(sc, "k_set", o);
    if (TRUE == sc_is_k_macro(sc, o))   return write_vector(sc, "k_macro", o);

    if (TRUE == sc_is_prim(sc, o)) {
        prim *p = object_to_prim(o,sc);
        printf("#prim<%p:%ld>", (void*)(p->fn),p->nargs);
        return VOID;
    }
    printf("#<%p>",(void*)o);
    return VOID;
}
object sc_post(sc* sc, object o) {
    sc_write(sc, o);
    printf("\n");
    return VOID;
}
/* This requires a trick since GC aborts and restarts the current primitive. */
object sc_gc(sc* sc) {
    state *s = CAST(state, sc->state);
    // Were sure it's a k_apply since we're a primitive application.
    k_apply *f = CAST(k_apply, s->continuation);
    sc->state = STATE(CLOSURE(VOID,NIL), f->parent);
    gc_collect(sc->gc);
    return NIL;
}
/* Like define, but the function version. */
object sc_setvar(sc* sc, object var, object val) {
    sc->toplevel = CONS(CONS(var,val), sc->toplevel);
    return VOID;
}
object sc_setmacro(sc* sc, object var, object val) {
    sc->toplevel_macro = CONS(CONS(var,val), sc->toplevel_macro);
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


/* Interpreter state contains only closures, but primitives and
   environments have either (fully reduced) closures or naked
   primitive values. */
static object closure_pack(sc *sc, object o, object cl) {
    if (NIL == cl) cl = CLOSURE(NIL,NIL);
    /* Use prealloc structure in-place.  This is used to make
       primitive side-effects play nice with GC pre-emption. */
    closure *c = object_to_closure(cl);
    if (TRUE==sc_is_closure(sc, o)) {
        closure *_c = object_to_closure(o);
        c->term = _c ->term;
        c->env  = _c ->env;
    }
    else {
        c->term = o;
        c->env  = NIL;
    }
    return cl;
}
static object closure_unpack(sc *sc, object cl) {
    closure *c = object_to_closure(cl);
    if (TRUE==sc_is_lambda(sc, c->term)) return cl;
    return c->term;
}

#define UP(x) closure_unpack(sc, x)
static inline object _sc_call(sc *sc, void *p, 
                              int nargs, object ra) {
    switch(nargs) {
    case 0: return ((sc_0)p)(sc);
    case 1: return ((sc_1)p)(sc, UP(CAR(ra)));
    case 2: return ((sc_2)p)(sc, UP(CADR(ra)), UP(CAR(ra)));
    case 3: return ((sc_3)p)(sc, UP(CADDR(ra)), UP(CADR(ra)), UP(CAR(ra)));
    default:
        return ERROR("prim", integer_to_object(nargs));
    }
}

/* Convert a list of terms obtained as part of an AST of a closure to
   a list of closures. */
object sc_close_args(sc *sc, object lst, object E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(CLOSURE(AST(CAR(lst)), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

object sc_interpreter_step(sc *sc, object o_state) {
    state *s = CAST(state, o_state);
    closure *c = CAST(closure, s->closure);
    object term = c->term;  // (open) term
    object env  = c->env;   // environment

    /* Abstract Syntax: perform a single reduction step.

       - create abstraction (lambda) value
       - create application continuation
       - perform variable reference
       - create special form contiuation (if, set!, macro, ...)
     */
    if (TRUE==sc_is_ast(sc, term)) {
        ast *stx = object_to_ast(term);
        object term = stx->datum;

        if (TRUE==sc_is_pair(sc, term)) {
            object term_f    = CAR(term);
            object term_args = CDR(term);

            /* Special Form */
            if (TRUE==sc_is_symbol(sc, term_f)) {
                if (term_f == sc->s_lambda) {
                    if (NIL == term_args) ERROR("syntax",term);
                    object formals = sc_list_to_vector(sc, CAR(term_args));
                    /* Implement the expression sequence in a `lambda'
                       expression as a `begin' sequencing form. */
                    // FIXME: don't do this if there's just 1 expr.
                    object stx = AST(CONS(sc->s_begin,
                                             CDR(term_args)));
                    return STATE(CLOSURE(LAMBDA(formals, stx), env),
                                 s->continuation);
                }
                if (term_f == sc->s_quote) {
                    if (NIL == term_args) ERROR("syntax",term);
                    return STATE(CLOSURE(CAR(term_args),env),
                                 s->continuation);
                }
                if (term_f == sc->s_if) {
                    if (NIL == term_args) ERROR("syntax",term);
                    if (NIL == CDR(term_args)) ERROR("syntax",term);
                    object cond = CLOSURE(AST(CAR(term_args)),env);
                    object yes  = CLOSURE(AST(CADR(term_args)),env);
                    object no   = 
                        (NIL == CDDR(term_args)) ? 
                        CLOSURE(VOID,NIL) :
                        CLOSURE(AST(CADDR(term_args)),env);
                    return STATE(cond,
                                 sc_make_k_if(sc,yes,no,
                                              s->continuation));
                }
                if (term_f == sc->s_setbang) {
                    if (NIL == term_args) ERROR("syntax",term);
                    if (NIL == CDR(term_args)) ERROR("syntax",term);
                    object var = CLOSURE(CAR(term_args),env);
                    object cl  = CLOSURE(AST(CADR(term_args)),env);
                    return STATE(cl, sc_make_k_set(sc, var, s->continuation));
                }
                if (term_f == sc->s_begin) {
                    if (NIL == term_args) ERROR("syntax",term);
                    object todo = sc_close_args(sc, term_args, env);
                    return STATE(CAR(todo),
                                 sc_make_k_seq(sc, CDR(todo),
                                               s->continuation));
                }
                object macro;
                if (FALSE != (macro = sc_find(sc, sc->toplevel_macro, term_f))) {
                    /* Macro continuation is based on a completed
                       k_apply frame that will trigger the fn
                       application, linked to a k_macro frame that
                       will steer the result back to the AST
                       reducer. */
                    object k_m = sc_make_k_macro(sc, c->env, s->continuation);
                    object k_a = sc_make_k_apply
                        (sc,  
                         CONS(closure_pack(sc, macro, NIL), NIL), // done list
                         NIL, // todo list
                         k_m);
                    return STATE(CLOSURE(term,NIL), k_a);
                }
            }

            /* Application Form */

            /* Extend the continuation with a new frame by collecting
               all (open) subterms, and binding them to the current
               environment. */
            object closed_args = sc_close_args(sc, term_args, env);
            return STATE(CLOSURE(AST(term_f), env),
                         sc_make_k_apply(sc, NIL,
                                         closed_args, 
                                         s->continuation));
        }
        /* Variable Reference */
        else if (TRUE==sc_is_symbol(sc, term)){
            object val; 
            if (FALSE == (val = sc_find(sc, env, term))) {
                if (FALSE == (val = sc_find_toplevel(sc, term))) {
                    return ERROR("undefined", term);
                }
            }
            return STATE(closure_pack(sc, val, NIL),  // wrap naked values
                         s->continuation);
        }
        /* Literal Value */
        else {
            return STATE(CLOSURE(term,env),
                         s->continuation);
        }
    }

    /* Value: depending on the type of continuation, we now figure out
       what to do with the value 
       
       - empty continuation: halt
       - argument evaluation: do next, or apply
       - macro expansion -> interpret the new term.
       - predicate position of an 'if' -> pick one
       - value position of 'set!' -> mutate environment
    */

    /* A fully reduced value in an empty continuation means the
       evaluation is finished, and the machine can be halted. */
    if (NIL == s->continuation) longjmp(sc->step, SC_EX_HALT);

    if (TRUE == sc_is_k_if(sc, s->continuation)) {
        k_if *k = object_to_k_if(s->continuation);
        object rc = (FALSE == c->term) ? k->no : k->yes;
        return STATE(rc, k->parent);
    }
    if (TRUE == sc_is_k_set(sc, s->continuation)) {
        k_set *k = object_to_k_set(s->continuation);
        closure *v = CAST(closure, k->var);
        // allocate before mutation
        object rv = STATE(CLOSURE(VOID,NIL), k->parent);
        object val = closure_unpack(sc, s->closure);
        if (FALSE == sc_env_set(sc, v->env, v->term, val)) {
            if (FALSE == sc_env_set(sc, sc->toplevel, v->term, val)) {
                return ERROR("undefined", v->term);
            }
        }
        return rv;
    }
    if (TRUE == sc_is_k_seq(sc, s->continuation)) {
        k_seq *k = object_to_k_seq(s->continuation);
        /* If there is another closure to reduce, discard current and
           pop next. */
        if (TRUE==sc_is_pair(sc, k->todo)) {
            return STATE(CAR(k->todo),
                         sc_make_k_seq(sc, 
                                       CDR(k->todo),
                                       k->parent));
        }
        /* If it's the last, keep it and discard the frame. */
        return STATE(s->closure, k->parent);
    }
    if (TRUE == sc_is_k_macro(sc, s->continuation)) {
        /* The object returned by the macro is wrapped as an AST wich
           triggers its further reduction. */
        k_macro *k = object_to_k_macro(s->continuation);
        return STATE(CLOSURE(AST(c->term),k->env), k->parent);
    }
    if (TRUE == sc_is_k_apply(sc, s->continuation)) {
        /* If there are remaining closures to evaluate, pop the
           next one and push the value to the update value list. */
        k_apply *k = object_to_k_apply(s->continuation);
        if (TRUE==sc_is_pair(sc, k->todo)) {
            return STATE(CAR(k->todo),
                         sc_make_k_apply(sc, 
                                         CONS(s->closure, k->done),
                                         CDR(k->todo), 
                                         k->parent));
        }
        /* No more expressions to be reduced in the current k_apply:
           perform application. */
        else {
            object rargs = CONS(s->closure, k->done);
            object p=rargs, fn;
            int n = 0;
            while (TRUE==sc_is_pair(sc,p)) {
                n++; fn = CAR(p); p = CDR(p);
            }
            // n  == 1 + nb_args
            // fn == primitive or lambda

            // unpack the closure
            closure *c = CAST(closure, fn);
            object fn_term = c->term;
            object fn_env  = c->env;

            /* Primitive functions are evaluated. */
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
                object closure = CLOSURE(NIL, NIL);
                object state   = STATE(closure, k->parent); // drop frame
                closure_pack(sc,
                             _sc_call(sc, prim_fn(p), n-1, rargs),
                             closure);
                return state;
            }
            /* Application extends the fn_env environment. */
            if (TRUE==sc_is_lambda(sc, fn_term)) {
                lambda *l = CAST(lambda, fn_term);
                vector *v = CAST(vector, l->formals);
                if (vector_size(v) != (n-1)) return ERROR("nargs", fn_term);
                int i;
                for (i=n-2; i>=0; i--) {
                    object cl = CAR(rargs);
                    object unpk = closure_unpack(sc, cl);
                    fn_env = CONS(CONS(v->slot[i], unpk), fn_env);
                    rargs = CDR(rargs);
                }
                return STATE(CLOSURE(l->term, fn_env),  // close term
                             k->parent);                // drop frame
            } 

            /* Unknown applicant type */
            return ERROR("apply", fn_term);
        }
    }
    /* Unknown continuation type */
    return ERROR("cont", s->continuation);
}
/* Convert an s-expression to a machine state that will eval and
   halt. */
object sc_datum_to_state(sc *sc, object expr) {
    object c = CLOSURE(expr,NIL);  // empty environment
    object k = NIL;                // empty continuation
    return STATE(c,k);
}


/* --- SETUP & GC --- */

void _sc_run(sc *sc){
    sc->entries++;
    int exception;
  next:
    switch (exception = setjmp(sc->step)) {
    case SC_EX_TRY:
        for(;;) {
            sc->state = sc_interpreter_step(sc, sc->state); 
        }
    case SC_EX_GC:    goto next;
    case SC_EX_ABORT: goto leave;
    case SC_EX_HALT:  goto leave;
    default:
        printf("Unknown exception %d.\n", exception);
        goto leave;
    }
  leave:
    sc->entries--;
}
/* Eval will run the machine until halt, which occurs for an
   irreducable closure and an empty continuation. */
object _sc_eval(sc *sc, object expr){
    sc->state = sc_datum_to_state(sc, expr);
    _sc_run(sc);
    state   *s = CAST(state, sc->state);
    // closure *c = CAST(closure, s->closure);
    sc->state  = NIL;
    // return c->term;
    return closure_unpack(sc, s->closure);
}
#define MARK(field) sc->field = gc_mark(sc->gc, sc->field)
static void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // sc_trap(sc);
    // printf("GC mark()\n");
    // sc_post(sc, sc->state);
    MARK(state);
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
    if (sc->entries) {
        longjmp(sc->step, SC_EX_GC);
    }
    printf("WARNING: triggering GC outside of the main loop.\n");
}
static object _sc_make_prim(sc *sc, void *fn, long nargs) {
    prim *p = malloc(sizeof(*p));
    p->a.op = &sc->op_prim; // class
    p->fn = fn;
    p->nargs = nargs;
    return atom_to_object(&p->a);
}
static void _sc_define_prim(sc *sc, object var, void *fn, long nargs) {
    sc_setvar(sc, var, _sc_make_prim(sc, fn, nargs));
}
#define DEF(str,fn,nargs) \
    _sc_define_prim (sc,SYMBOL(str),fn,nargs)

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

    /* Cached identifiers */
    sc->s_lambda  = SYMBOL("lambda");
    sc->s_if      = SYMBOL("if");
    sc->s_setbang = SYMBOL("set!");
    sc->s_quote   = SYMBOL("quote");
    sc->s_begin   = SYMBOL("begin");

    /* Primitive defs */
#include "scheme_prim.inc"
    return sc;
}



