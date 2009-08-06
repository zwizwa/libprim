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
object sc_is_syntax(sc *sc, object o)  { return vector_type(o, TAG_SYNTAX); }

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
object sc_make_syntax(sc *sc, object datum){
    object o = gc_vector(sc->gc, 1, datum);
    vector_set_tag(o, TAG_SYNTAX);
    return o;
}


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
    if (TRUE == sc_is_syntax(sc, o))  return write_vector(sc, "syntax", o);
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

object sc_close_args(sc *sc, object lst, object E) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(CLOSURE(SYNTAX(CAR(lst)), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

object sc_interpreter_step(sc *sc, object o_state) {
    state *s = CAST(state, o_state);
    closure *c = CAST(closure, s->closure);
    object term = c->term;  // (open) term
    object env  = c->env;   // environment

    /* Syntax */
    if (TRUE==sc_is_syntax(sc, term)) {
        syntax *stx = object_to_syntax(term);
        object term = stx->datum;

        if (TRUE==sc_is_pair(sc, term)) {
            object term_f    = CAR(term);
            object term_args = CDR(term);

            /* Special Form */
            if (TRUE==sc_is_symbol(sc, term_f)) {
                if (term_f == sc->s_lambda) {
                    object formals = sc_list_to_vector(sc, CAR(term_args));
                    object stx = SYNTAX(CADR(term_args));
                    return STATE(CLOSURE(LAMBDA(formals, stx), env),
                                 s->continuation);
                }
                // if (X_f == sc->s_if) {}
                // if (X_f == sc->s_set) {}
            }

            /* Application Form */

            /* Extend the continuation with a new frame by collecting
               all (open) subterms, and binding them to the current
               environment. */
            object closed_args = sc_close_args(sc, term_args, env);
            return STATE(CLOSURE(SYNTAX(term_f), env),
                         FRAME(NIL, 
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

    /* Value */
    else {
        /* A fully reduced value in an empty continuation means the
           evaluation is finished, and the machine can be halted. */
        if (NIL == s->continuation) longjmp(sc->step, SC_EX_HALT);


        /* If there are remaining closures to evaluate, pop the
           next one and push the value to the update value list. */
        frame *f = CAST(frame, s->continuation);
        if (TRUE==sc_is_pair(sc, f->todo)) {
            return STATE(CAR(f->todo),
                         FRAME(CONS(s->closure, f->done),
                               CDR(f->todo), 
                               f->parent));
        }
        /* No more expressions to be reduced in the current frame:
           perform application. */
        else {
            object rargs = CONS(s->closure, f->done);
            object p=rargs, fn;
            int n = 0;
            while (TRUE==sc_is_pair(sc,p)) {
                n++; fn = CAR(p); p = CDR(p);
            }
            // n    == 1 + nb_args
            // V_fn == primitive or lambda

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
                object state   = STATE(closure, f->parent); // drop frame
                closure_pack(sc,
                             _sc_call(sc, prim_fn(p), n-1, rargs),
                             closure);
                return state;
            }
            /* Application extends the fn_env environment. */
            if (TRUE==sc_is_lambda(sc, fn_term)) {
                lambda *l = CAST(lambda, fn_term);
                vector *v = CAST(vector, l->formals);
                if (vector_size(v) != (n-1)) {
                    return ERROR("nargs", fn_term);
                }
                int i;
                for (i=n-2; i>=0; i--) {
                    object cl = CAR(rargs);
                    if (FALSE == sc_is_closure(sc, cl)) ERROR("env-type", cl);
                    fn_env = CONS(CONS(v->slot[i], 
                                       closure_unpack(sc, cl)),
                                  fn_env);
                    rargs = CDR(rargs);
                }
                return STATE(CLOSURE(l->term, fn_env),  // close term
                             f->parent);                // drop frame
            } 
            else {
                return ERROR("apply", fn_term);
            }
        }
    }
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
        // sc_write(sc, sc->state); sc_newline(sc);
        sc->state = sc_interpreter_step(sc, sc->state); 
        goto next;
    case SC_EX_GC:
        printf("GC restart.\n");
        goto next;
    case SC_EX_ABORT:
        // printf("Abort.\n");
        goto leave;
    case SC_EX_HALT:
        // printf("Halt.\n");
        goto leave;
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
    printf("GC mark()\n");
    sc_post(sc, sc->state);
    MARK(state);
    MARK(toplevel);
    fin(sc->gc);
    sc_post(sc, sc->state);
    /* Abort C stack, since it now contains invalid refs. */
    if (sc->entries) {
        longjmp(sc->step, SC_EX_GC);
    }
    printf("WARNING: GC triggered outside of mainloop.\n");
}
static object _sc_make_prim(sc *sc, void *fn, long nargs) {
    prim *p = malloc(sizeof(*p));
    p->a.op = &sc->op_prim; // class
    p->fn = fn;
    p->nargs = nargs;
    return atom_to_object(&p->a);
}
static void _sc_define_prim(sc *sc, object var, void *fn, long nargs) {
    object prim = _sc_make_prim(sc, fn, nargs);
    sc->toplevel = CONS(CONS(var, CLOSURE(prim, NIL)),
                        sc->toplevel);
}
#define DEF(str,fn,nargs) \
    _sc_define_prim (sc,SYMBOL(str),fn,nargs)

#define DEFSYM(name) sc->s_##name = SYMBOL(#name)
sc *_sc_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->entries = 0;
    sc->gc = gc_new(200, (gc_mark_roots)_sc_mark_roots, sc);
    sc->syms = symstore_new(1000);
    DEFSYM(lambda);
    DEFSYM(if);
    sc->toplevel = NIL;
    sc->op_prim.free = NULL;
#include "scheme_prim.inc"
    return sc;
}



