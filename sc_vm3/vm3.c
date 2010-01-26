#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include <config.h>

#include <sc_vm3/vm3.h>
#include <sc_vm3/vm3.h_prims>
#include <ex/ex.h_prims>
#include <sc/sc.h_prims>



/* A treewalking interpreter implemented as a CEK machine w/o
   compiler. */



/* Predicates */
_ sc_is_lambda(sc *sc, _ o)  { return _is_vector_type(o, TAG_LAMBDA); }
_ sc_is_state(sc *sc, _ o)   { return _is_vector_type(o, TAG_STATE); }
_ sc_is_redex(sc *sc, _ o)   { return _is_vector_type(o, TAG_REDEX); }
_ sc_is_value(sc *sc, _ o)   { return _is_vector_type(o, TAG_VALUE); }

_ sc_is_k_if(sc *sc, _ o)    { return _is_vector_type(o, TAG_K_IF); }
_ sc_is_k_args(sc *sc, _ o)  { return _is_vector_type(o, TAG_K_ARGS); }
_ sc_is_k_seq(sc *sc, _ o)   { return _is_vector_type(o, TAG_K_SEQ); }
_ sc_is_k_set(sc *sc, _ o)   { return _is_vector_type(o, TAG_K_SET); }

_ sc_is_k(sc *sc, _ o) {
    vector *v;
    if (MT == o) return TRUE;
    if ((v = object_to_vector(o))) {
        if (flags_is_k(vector_to_flags(v))) return TRUE;
    }
    return FALSE;
}
_ sc_k_parent(sc *sc, _ o) {
    vector *v;
    if (MT == o) TYPE_ERROR(o);
    if ((v = object_to_vector(o))) {
        if (flags_is_k(vector_to_flags(v))) return v->slot[0];
    }
    return TYPE_ERROR(o);
}


/* Constructors */
// C = closure
// K = continuation
// F = formal argument vector
// R = rest args
// S = syntax term (REDEX)
// D = done (list of reduced closures)
// T = todo (list of non-reduced closures)
// E = environment (Et = toplevel)
// M = macro environment (Et = toplevel)
// P = parent continuation
// D = datum


_ sc_make_state(sc *sc, _ C, _ K)                 {return STRUCT(TAG_STATE,   2, C,K);}
_ sc_make_lambda(sc *sc, _ F, _ R, _ S, _ E)      {return STRUCT(TAG_LAMBDA,  4, F,R,S,E);}
_ sc_make_redex(sc *sc, _ D, _ E)                 {return STRUCT(TAG_REDEX,   2, D,E);}
_ sc_make_value(sc *sc, _ D)                      {return STRUCT(TAG_VALUE,   1, D);}


// 'P' is in slot 0
// continuations are created with an empty mark list
_ sc_make_k_args(sc *sc, _ P, _ D, _ T)      {return STRUCT(TAG_K_ARGS,   4, P,NIL,D,T);}
_ sc_make_k_if(sc *sc, _ P, _ Y, _ N)        {return STRUCT(TAG_K_IF,     4, P,NIL,Y,N);}
_ sc_make_k_set(sc *sc, _ P, _ V, _ E, _ Et) {return STRUCT(TAG_K_SET,    5, P,NIL,V,E,Et);}
_ sc_make_k_seq(sc *sc, _ P, _ T)            {return STRUCT(TAG_K_SEQ,    3, P,NIL,T);}

_ sc_make_mt(sc *sc)    { return MT; }



_ sc_abort_k(sc *sc)        { _GLOBAL(abort_k); }



_ sc_write_stderr(sc *sc,  _ o) {
    vector *v = object_to_vector(o);
    if (TRUE == sc_is_state(sc, o))   return _ex_write_vector(EX, "state", v);
    if (TRUE == sc_is_redex(sc, o))   return _ex_write_vector(EX, "redex", v);
    if (TRUE == sc_is_value(sc, o))   return _ex_write_vector(EX, "value", v);
    if (TRUE == sc_is_error(sc, o))   return _ex_write_vector(EX, "error", v);

    if (TRUE == sc_is_k_args(sc, o))  return _ex_write_vector(EX, "k_args", v);
    if (TRUE == sc_is_k_if(sc, o))    return _ex_write_vector(EX, "k_if", v);
    if (TRUE == sc_is_k_seq(sc, o))   return _ex_write_vector(EX, "k_seq", v);
    if (TRUE == sc_is_k_set(sc, o))   return _ex_write_vector(EX, "k_set", v);
    if (MT   == o) { _ex_printf(EX, "#k_mt"); return VOID; }

    if (TRUE == sc_is_lambda(sc, o)) {
        // return _ex_write_vector(EX, "lambda", v);
        // code might contain cycles..
        _ex_printf(EX, "#<procedure>", _sc_port(sc));
        return VOID;
    }
    return _ex_write(EX, o);
}










#define NARGS_ERROR(fn) ERROR("nargs", fn)



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


/* Propagate environment during reduction. */
_ sc_close_args(sc *sc, _ lst, _ E) {
    if ((TRUE==IS_NULL(lst))) return NIL;
    else return CONS(REDEX(CAR(lst), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

_ sc_error_undefined(sc *sc, _ o) { return ERROR("undefined", o); }

_ _sc_step_value(sc *sc, _ v, _ k) {

    /* Look at the continuation to determine what to do with the value. 
       
       - empty continuation -> halt
       - argument evaluation -> eval next, or apply
       - predicate position of an 'if' -> pick yes or no
       - value position of 'set!' -> mutate environment
       - ...
    */

    /* Unwrap */
    value *vx = CAST(value, v);
    _ value = vx->datum;

    /* A fully reduced value in an empty continuation means the
       evaluation is finished, and the machine can be halted. */
    if (MT == k) HALT_VM(value);

    if (TRUE == sc_is_k_if(sc, k)) {
        k_if *kx = object_to_k_if(k);
        _ rc = (FALSE == value) ? kx->no : kx->yes;
        return STATE(rc, kx->k.parent);
    }
    if (TRUE == sc_is_k_set(sc, k)) {
        k_set *kx = object_to_k_set(k);
        // allocate before mutation
        _ rv = STATE(VALUE(VOID), kx->k.parent);
        if (FALSE == ENV_SET(kx->env, kx->var, value)) {
            if (FALSE == ENV_SET(sc_global(sc, kx->tl_slot),  // global toplevel
                                 kx->var, value)) {
                return ERROR_UNDEFINED(kx->var);
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
    if (TRUE == sc_is_k_args(sc, k)) {
        /* If there are remaining closures to evaluate, push the value
           to the update value list and pop the next closure. */
        k_args *kx = object_to_k_args(k);
        if (TRUE==IS_PAIR(kx->todo)) {
            return STATE(CAR(kx->todo),
                         sc_make_k_args(sc, kx->k.parent,
                                        CONS(value, kx->done),
                                        CDR(kx->todo)));
        }
        /* No more expressions to be reduced in the current k_apply:
           perform application. */
        else {
            _ fn   = value;
            _ args = kx->done; 

            // fn == primitive | lambda | continuation

            /* Application of primitive function results in C call. */
            if (TRUE==IS_PRIM(fn)) {
                prim *p = object_to_prim(fn);
                sc->m.prim = p; // for debug

                /* Before entering primitive code, make GC restarts
                   illegal.  Code that allocates a large amount of
                   cells needs to re-enable restarts explicitly.  A
                   small number of cells are guaranteed to exist. */
                _ value = VALUE(VOID);
                _ state = STATE(value, kx->k.parent);
                EX->stateful_context = 1;
                _ rv = _sc_call(sc, prim_fn(p), prim_nargs(p), args);
                object_to_value(value)->datum = rv;
                return state;
            }

            /* Application of abstraction extends the fn_env environment. */
            if (TRUE==sc_is_lambda(sc, fn)) {
                lambda *l = CAST(lambda, fn);
                vector *v = CAST(vector, l->formals);
                long    n = vector_size(v);
                _  fn_env = l->env;

                /* Extend environment */
                int i;
                for (i = 0; i < n; i ++) {
                    fn_env = CONS(CONS(v->slot[i], CAR(args)), fn_env);
                    args = CDR(args);
                }
                if (NIL != l->rest) {
                    fn_env = CONS(CONS(l->rest, args), fn_env);
                }
                else {
                    if (args != NIL) return ERROR("nargs", fn);
                }
                return STATE(REDEX(l->term, fn_env),  // close term
                             kx->k.parent);           // drop frame
            } 

            /* Continuation */
            if (TRUE==sc_is_k(sc, fn)) {
                _ arg;
                pair *p = object_to_pair(args);
                if (!p) { 
                    arg = VOID; // no args to k -> inserts void.
                }
                else {
                    arg = p->car;
                    if (p->cdr != NIL) ERROR("nargs", fn);
                }
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

    _ term;      // C
    _ env;       // E
    _ k;         // K

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
    if (TRUE==IS_SYMBOL(term)){
        // fprintf(stderr, " %s", object_to_symbol(term)->name);
        _ slot;
        if (FALSE == (slot = FIND_SLOT(env, term))) {
            if (FALSE == (slot = FIND_SLOT(TOPLEVEL(), term))) {
                return ERROR_UNDEFINED(term);
            }
        }
        return STATE(VALUE(CDR(slot)), k);
    }

    /* Literal Value */
    if (FALSE==IS_PAIR(term)) {
        _ val = (TRUE==sc_is_lambda(sc, term)) 
            ? s->redex_or_value : term;

        return STATE(VALUE(val),k);
    }

    _ term_f    = CAR(term);
    _ term_args = CDR(term);

    /* Special Form */
    if (TRUE==IS_SYMBOL(term_f)) {
        if (term_f == ((sc_interpreter*)sc)->s_lambda) {
            if (NIL == term_args) goto syntax_error;
            _ argspec = CAR(term_args);
            _ named;
            _ rest;
            _ex_length_rest(EX, argspec, &named, &rest);
            if ((NIL   != rest) &&
                (FALSE == IS_SYMBOL(rest))) {
                goto syntax_error;
            }
            _ formals = TAKE_VECTOR(named, argspec);
            /* Implement the expression sequence in a `lambda'
               expression as a `begin' sequencing form. */
            _ body = CDR(term_args);
            if (NIL == CDR(body)) body = CAR(body);
            else body = CONS(((sc_interpreter*)sc)->s_begin, body);
            _ l = sc_make_lambda(sc, formals, rest, body, env);
            return STATE(VALUE(l), k);
        }
        if (term_f == ((sc_interpreter*)sc)->s_quote) {
            if (NIL == term_args) goto syntax_error;
            return STATE(VALUE(CAR(term_args)), k);
        }
        if (term_f == ((sc_interpreter*)sc)->s_if) {
            if (NIL == term_args) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ cond = REDEX(CAR(term_args),env);
            _ yes  = REDEX(CADR(term_args),env);
            _ no   = 
                (NIL == _CDDR(term_args)) ? 
                VALUE(VOID) :
                REDEX(_CADDR(term_args),env);
            return STATE(cond, sc_make_k_if(sc, k, yes,no));
                                              
        }
        if (term_f == ((sc_interpreter*)sc)->s_bang_set) {
            if (NIL == term_args) goto syntax_error;
            _ var = CAR(term_args);
            if (FALSE == IS_SYMBOL(var)) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ expr = CADR(term_args);
            return STATE(REDEX(expr, env),
                         sc_make_k_set(sc, k, var, env, sc_slot_toplevel));
        }
        if (term_f == ((sc_interpreter*)sc)->s_begin) {
            /* (begin) is a NOP */
            if (NIL == term_args) return STATE(VALUE(VOID), k);
            // if (FALSE == IS_PAIR(term_args)) goto syntax_error;
            _ todo = sc_close_args(sc, term_args, env);
            pair *body = object_to_pair(todo);
            /* Don't create a contination frame if there's only a
               single expression. */
            if (NIL == body->cdr) return STATE(body->car, k);
            return STATE(body->car, sc_make_k_seq(sc, k, body->cdr));
        }                
        if (term_f == ((sc_interpreter*)sc)->s_letcc) {
            if (NIL == term_args) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ var = CAR(term_args);
            env   = CONS(CONS(var,k),env);
            _ cl  = REDEX(CADR(term_args),env);
            return STATE(cl, k);
        }
        /* Fallthrough: symbol must be bound to applicable values. */
    }

    /* Application */

    /* Extend the continuation with a new frame by collecting all
       (open) subterms, and binding them to the current environment.
       The terms are stored in reverse order: evaluation is right to
       left, which leads to simpler data structures. */

    
    _ closed = sc_close_args(sc, REVERSE(term), env);
    return STATE(CAR(closed),
                 sc_make_k_args(sc, k, NIL, 
                                CDR(closed)));
  syntax_error:
    return ERROR("syntax",term);
}





void _sc_pop_k(sc *sc, _ value) {
    state *s = CAST(state, sc_global(sc, sc_slot_state));
    _ k = sc_k_parent(sc, s->continuation); // drop `gc' k_apply frame
    sc_bang_set_global(sc, sc_slot_state, STATE(value, k)); // update state manually    
}


/* GC: set continuation manually, since since the interpreter aborts
   and restarts the current step. */
_ sc_gc(sc* sc) {
    _sc_pop_k(sc, VALUE(VOID));
    EX->stateful_context = 0;  // enable restarts
    gc_collect(sc->m.gc);      // collect will restart at sc->state
    return NIL; // not reached
}

_ sc_gc_used(sc *sc) {
    return integer_to_object(sc->m.gc->current_index);
}


/* Yield is currently implemented as halt.  Essentially it pops the
   continuation frame that contains the sc_yield primitive application. */
_ sc_yield(sc *sc, _ ob) {
    _sc_pop_k(sc, VALUE(ob));
    return ex_halt_vm(EX, FALSE);
}



/* A ktx allows modification of a continuation frame.  The result can
   be invoked as a continuation.  FIXME: change these to primitives
   that update VM state. */
_ sc_apply_ktx(sc* sc, _ k, _ args) {
    return sc_make_k_args(sc, k, args, NIL);
}
_ sc_eval_ktx(sc *sc, _ k, _ expr) {
    // sc_write_stderr(sc, expr);
    return sc_make_k_seq(sc, k, CONS(REDEX(expr, NIL),NIL));
}

_ sc_bang_abort_k(sc *sc, _ k) {
    return sc_bang_set_global(sc, sc_slot_abort_k, k);
}


/* --- SETUP & GC --- */





void _sc_loop(sc *sc) {
    _ in_state;
    for(;;) {
        /* From here to the invocation of primitive code it is OK
           to perform a restart when a garbage collection occurs.
           We guarantee a minimum amount of free cells to
           primitive code, and will trigger collection here in
           case there is not enough. */
        EX->stateful_context = 0;
        if (gc_available(EX->gc) < EX->gc_guard_cells) gc_collect(EX->gc);
        
        /* The _sc_step() function will perform a single state
           update, or exit through sc->m.except in which case
           the sc_slot_state field is not updated. */
        in_state = sc_global(sc, sc_slot_state);
        sc_bang_set_global(sc, sc_slot_state, 
                           _sc_step(sc, in_state));
    }
}

void _sc_abort(sc *sc) {
    sc_bang_set_global(sc, sc_slot_state,
                       STATE(VALUE(sc->error), 
                             sc_global(sc, sc_slot_abort_k)));
}

/* Run the above loop in a dynamic context. */
_ _sc_continue(sc *sc) { 
    return _sc_continue_dynamic(sc, _sc_loop, _sc_abort); 
}


/* Set the current VM state to start evaluating an expression on _sc_continue() */
void _sc_prepare(sc *sc, _ expr) {
    sc_bang_set_global(sc, sc_slot_state, STATE(REDEX(expr,NIL),MT));
}

_ _sc_top(sc *sc, _ expr) {
    _sc_prepare(sc, expr);
    return _sc_continue(sc);
}

static prim_def scheme_prims[] = vm3_table_init;



sc *_sc_new(int argc, const char **argv) {
    sc *sc = calloc(1, sizeof(sc_interpreter));
    sc_bootinfo info;
    if (_sc_init(sc, argc, argv, &info)) {
        free(sc);
        return NULL;
    }

    /* Cached identifiers for use in interpreter. */
    ((sc_interpreter*)sc)->s_lambda   = SYMBOL("%lambda");
    ((sc_interpreter*)sc)->s_if       = SYMBOL("if");
    ((sc_interpreter*)sc)->s_bang_set = SYMBOL("set!");
    ((sc_interpreter*)sc)->s_quote    = SYMBOL("quote");
    ((sc_interpreter*)sc)->s_begin    = SYMBOL("begin");
    ((sc_interpreter*)sc)->s_letcc    = SYMBOL("letcc");

    /* Primitive defs */
    _sc_def_prims(sc, scheme_prims);
    
    /* Printer for interpreter's tagged vectors. */
    sc->m.write = (ex_m_write)sc_write_stderr;


    /* Toplevel abort continuation */
    _ done = CONS(FIND(TOPLEVEL(),SYMBOL("print-error")),NIL);
    _ abort_k = sc_make_k_args(sc, MT, done, NIL);

    sc_bang_abort_k(sc, abort_k);


    _sc_top(sc, _ex_boot_load(EX, info.bootfile));

    /* Set the continuation to continue booting in Scheme when the vm
       is started using _sc_continue() */
    _sc_prepare(sc, CONS(SYMBOL("eval-string"),
                    CONS(STRING(info.evalstr),
                    NIL)));

    return sc;
}


/* FIXME: currently this doesn't save the existing I/O ports (only for
   headless embedding). */

const char *_sc_repl_cstring(sc *sc, const char *commands) {
    bytes *bin = bytes_from_cstring(commands);
    bytes *bout = bytes_buffer_new(1000);
    _ in  = _ex_make_bytes_port(EX, bin);
    _ out = _ex_make_bytes_port(EX, bout);
    sc_bang_set_global(sc, sc_slot_input_port,  in);
    sc_bang_set_global(sc, sc_slot_output_port, out);
    sc_bang_set_global(sc, sc_slot_error_port,  out);
    _sc_top(sc, CONS(SYMBOL("repl-oneshot"), NIL));
    const char *output = cstring_from_bytes(bout);
    return output;
}

/* Coroutine interface bwetween C caller and Scheme VM.  From the C
 * side, this maps a string to a string: no scheme values pass this
 * barrier. */

// const char *_sc_


void _sc_eval_cstring(sc *sc, const char *commands) {
    _sc_top(sc, CONS(SYMBOL("eval-string"), CONS(STRING(commands), NIL)));
}

const char *_sc_yield(sc *sc, const char *msg) {
    state *s;
    s = CAST(state, sc_global(sc, sc_slot_state));
    s->redex_or_value = VALUE(_sc_make_aref(sc, bytes_from_cstring(msg)));

    _sc_continue(sc);

    s = CAST(state, sc_global(sc, sc_slot_state));
    value *v = object_to_value(s->redex_or_value);
    if (!v) {
        fprintf(stderr, "YIELD gives redex, not value!\n");
        exit(1);
    }
    return object_to_cstring(v->datum);
}


#include <leaf/console.h>
// #include <pthread.h>




#define QUOTE(x) CONS(SYMBOL("quote"), CONS(x, NIL))

console *_sc_prepare_console_server(sc *sc, const char *node, int port) {

    /* Create bi-directional pipe objects. */
    int to_vm[2];    // 0 = READ, 1 = WRITE
    int from_vm[2];
    pipe(to_vm);    
    pipe(from_vm);
    console *c = console_new(port_file_new(fdopen(from_vm[0], "r"), "from-vm"),
                             port_file_new(fdopen(to_vm[1], "w"), "to-vm"));
    _ io = CONS(_ex_make_file_port(EX, fdopen(to_vm[0], "r"), "from-console"),
                _ex_make_file_port(EX, fdopen(from_vm[1], "w"), "to-console"));

    /* Set continuation.  Call _sc_resume() to invoke. */


    _ addr = (port == 0) 
        ? STRING(node)  // Unix socket
        : CONS(STRING(node), CONS(integer_to_object(port), NIL)); // TCP socket

    _sc_prepare(sc, CONS(SYMBOL("init-console"), 
                    CONS(QUOTE(io),
                    CONS(QUOTE(addr),
                    NIL))));

    fprintf(stderr, "prepared console %s:%d\n", node, port);

    return c;
}





