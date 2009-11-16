#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "scheme.h"
#include "../config.h"

// generated
#include "scheme.h_sc_prims"
#include "../ex/ex.h_ex_prims"


/* --- PRIMITIVES --- */

/* To simplify the implementation, most C functions are implemented as
   Scheme primitives operating on Scheme values.  They use the prefix
   "sc_".

   Note in particular that interpreter data constructors are available
   in Scheme, and that "sc_eval_step()" is re-entrant with primitive
   errors limited to the innermost step.

   The functions operating on *sc that are too lowlevel to respect the
   "sc_" ABI (because they use values that cannot be represented as a
   Scheme object, or because they otherwize violate behavioural
   constraints) are prefixed "_sc".  These functions are kept to a
   minimum.
*/


/* Predicates for primitive objects are derived from their
   object_to_pointer cast: if it returns NULL, the type isn't
   correct. */
#define OBJECT_PREDICATE(cast) \
    {if (cast(o, &sc->m)) return TRUE; else return FALSE;}
// _ _DISABLED_sc_is_ck(sc *sc, _ o)     { OBJECT_PREDICATE(object_to_ck); }
_ sc_is_port(sc *sc, _ o)   { OBJECT_PREDICATE(object_to_port); }
_ sc_is_bytes(sc *sc, _ o)  { OBJECT_PREDICATE(object_to_bytes); }




/* Predicates */
_ sc_is_lambda(sc *sc, _ o)  { return _is_vector_type(o, TAG_LAMBDA); }
_ sc_is_state(sc *sc, _ o)   { return _is_vector_type(o, TAG_STATE); }
_ sc_is_redex(sc *sc, _ o)   { return _is_vector_type(o, TAG_REDEX); }
_ sc_is_value(sc *sc, _ o)   { return _is_vector_type(o, TAG_VALUE); }
_ sc_is_error(sc *sc, _ o)   { return _is_vector_type(o, TAG_ERROR); }
_ sc_is_aref(sc *sc, _ o)    { return _is_vector_type(o, TAG_AREF); }

_ sc_is_k_if(sc *sc, _ o)    { return _is_vector_type(o, TAG_K_IF); }
_ sc_is_k_apply(sc *sc, _ o) { return _is_vector_type(o, TAG_K_APPLY); }
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


_ sc_make_state(sc *sc, _ C, _ K)                 {STRUCT(TAG_STATE,   2, C,K);}
_ sc_make_lambda(sc *sc, _ F, _ R, _ S, _ E)      {STRUCT(TAG_LAMBDA,  4, F,R,S,E);}
_ sc_make_error(sc *sc, _ T, _ A, _ K, _ X)       {STRUCT(TAG_ERROR,   4, T,A,K,X);}
_ sc_make_redex(sc *sc, _ D, _ E)                 {STRUCT(TAG_REDEX,   2, D,E);}
_ sc_make_value(sc *sc, _ D)                      {STRUCT(TAG_VALUE,   1, D);}
_ sc_make_aref(sc *sc, _ F, _ O)                  {STRUCT(TAG_AREF,    2, F,O);}


// 'P' is in slot 0
// continuations are created with an empty mark list
_ sc_make_k_apply(sc *sc, _ P, _ D, _ T)     {STRUCT(TAG_K_APPLY,  4, P,NIL,D,T);}
_ sc_make_k_if(sc *sc, _ P, _ Y, _ N)        {STRUCT(TAG_K_IF,     4, P,NIL,Y,N);}
_ sc_make_k_set(sc *sc, _ P, _ V, _ E, _ Et) {STRUCT(TAG_K_SET,    5, P,NIL,V,E,Et);}
_ sc_make_k_seq(sc *sc, _ P, _ T)            {STRUCT(TAG_K_SEQ,    3, P,NIL,T);}


/* Wrap a leaf object in an aref struct.  The destructor is gathered
   from the leaf_class.  Note that GC finalizers are pointers to
   function pointers (this is because function pointers themselves
   might not be aligned, and thus have no space for bit tags. */

_ _sc_make_aref(sc *sc, void *_x) {
    leaf_object *x = _x;
    if (!x) ERROR("aref", VOID);
    fin *f = (fin*)((void*)(&x->methods->free));
    return sc_make_aref(sc, fin_to_object(f), const_to_object(x));
}

_ _sc_make_string(sc *sc, const char *str) {
    return _sc_make_aref(sc, bytes_from_cstring(str));
}
_ _sc_make_qstring(sc *sc, const char *str) {
    return _sc_make_aref(sc, bytes_from_qcstring(str));
}
_ _sc_make_bytes(sc *sc, int size) {
    return _sc_make_aref(sc, bytes_new(size));
}
_ sc_number_to_string(sc *sc, _ num) {
    char str[20];
    sprintf(str, "%ld", (long int)CAST_INTEGER(num));
    return _sc_make_string(sc, str);
}

_ sc_bytes_ref(sc *sc, _ ob_bytes, _ ob_index) {
    bytes *b = CAST(bytes, ob_bytes);
    int i = CAST_INTEGER(ob_index);
    if ((i < 0) || (i >= b->size)) return ERROR("index", ob_index);
    return integer_to_object(b->bytes[i]);
}

_ sc_make_mt(sc *sc)    { return MT; }

_ sc_global(sc *sc, _ n) { 
    return VECTOR_REF(sc->global, n); 
}
_ sc_bang_set_global(sc *sc, _ n, _ val) { 
    return BANG_VECTOR_SET(sc->global, n, val); 
}

#define _GLOBAL(name) return sc_global(sc, sc_slot_##name)
#define _GLOBAL_SET(name, val) return sc_bang_set_global(sc, sc_slot_##name, val)

_ sc_toplevel(sc *sc)       { _GLOBAL(toplevel); }
_ sc_toplevel_macro(sc *sc) { _GLOBAL(toplevel_macro); }
_ sc_machine_state(sc *sc)  { _GLOBAL(state); }
_ sc_abort_k(sc *sc)        { _GLOBAL(abort_k); }

_ sc_bang_set_toplevel(sc *sc, _ val)       { _GLOBAL_SET(toplevel, val); }
_ sc_bang_set_toplevel_macro(sc *sc, _ val) { _GLOBAL_SET(toplevel_macro, val); }

/*  Add to or mutate toplevel env. */
_ sc_bang_def_global(sc* sc, _ slot, _ var, _ val) {
    symbol *s;
    _ env = sc_global(sc, slot);
    if (!(s=object_to_symbol(var, &sc->m))) TYPE_ERROR(var);
    // _ex_printf(EX, "DEF %s: \n",s->name); // sc_write(EX, val);
    sc_bang_set_global(sc, slot, ENV_DEF(env, var, val));
    return VOID;
}
_ sc_bang_def_toplevel(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel, var, val);
}
_ sc_bang_def_toplevel_macro(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel_macro, var, val);
}

// FIXME: should be parameter
port *_sc_port(sc *sc) {
    return object_to_port(CURRENT_ERROR_PORT(), EX);
}
_ sc_current_error_port(sc *sc)  { return sc_global(sc, sc_slot_error_port); }
_ sc_current_input_port(sc *sc)  { return sc_global(sc, sc_slot_input_port); }
_ sc_current_output_port(sc *sc) { return sc_global(sc, sc_slot_output_port); }

_ sc_bytes_dump(sc *sc, _ ob) {
    bytes_dump(CAST(bytes, ob), _sc_port(sc));
    return VOID;
}

_ sc_write_bytes(sc *sc, _ ob_bytes, _ ob_port) {
    port *p = CAST(port, ob_port);
    bytes *b = CAST(bytes, ob_bytes);
    if (b->size != port_write(p, b->bytes, b->size)) {
        return ERROR("fwrite", ob_bytes);
    }
    return VOID;
}



_ sc_write_port(sc *sc, _ o, _ o_port) {
    /* This `dynamic-wind' hack only works because we're sure there
       are no aborts in this dynamic extent!  FIXME: use explicit
       lexical variables where possible. */
    port *p = CAST(port, o_port);
    _ saved_port = sc_global(sc, sc_slot_error_port);
    sc_bang_set_global(sc, sc_slot_error_port, o_port);
    _ rv = sc_write_stderr(sc, o);
    sc_bang_set_global(sc, sc_slot_error_port, saved_port);
    return rv;
}

_ sc_write_stderr(sc *sc,  _ o) {
    void *x;
    aref* a;

    /* FIXME: Bytes and strings are currently the same. */
    if ((x = object_to_port(o, EX)) ||
        (x = object_to_bytes(o, EX))) {
        return _ex_write(EX, const_to_object(x));
    }

    /* If an aref object's class has a lowlevel write method defined, call it. */
    if ((a = object_to_aref(o))) {
        leaf_object *x = object_to_const(a->object);
        if (x->methods->write) {
            x->methods->write(x, _sc_port(sc));
            return VOID;
        }
    }

    vector *v = object_to_vector(o);
    if (TRUE == sc_is_state(sc, o))   return _ex_write_vector(EX, "state", v);
    if (TRUE == sc_is_lambda(sc, o))  return _ex_write_vector(EX, "lambda", v);
    if (TRUE == sc_is_redex(sc, o))   return _ex_write_vector(EX, "redex", v);
    if (TRUE == sc_is_value(sc, o))   return _ex_write_vector(EX, "value", v);
    if (TRUE == sc_is_error(sc, o))   return _ex_write_vector(EX, "error", v);
    // if (TRUE == sc_is_aref(sc, o))    return _ex_write_vector(EX, "aref", o);

    if (TRUE == sc_is_k_apply(sc, o)) return _ex_write_vector(EX, "k_apply", v);
    if (TRUE == sc_is_k_if(sc, o))    return _ex_write_vector(EX, "k_if", v);
    if (TRUE == sc_is_k_seq(sc, o))   return _ex_write_vector(EX, "k_seq", v);
    if (TRUE == sc_is_k_set(sc, o))   return _ex_write_vector(EX, "k_set", v);
    if (MT   == o) { _ex_printf(EX, "#k_mt"); return VOID; }

    return _ex_write(EX, o);
}

_ sc_read_char(sc *sc) {
    return integer_to_object(fgetc(stdin));
}

// FIXME: factor this out to incorportate high level errors
_ sc_print_error(sc *sc, _ err) {
    if (TRUE == sc_is_error(sc, err)) {
        error *e = object_to_error(err);
        _ex_printf(EX, "ERROR");
        if (TRUE == IS_PRIM(e->prim)) {
            prim *p = object_to_prim(e->prim, &sc->m);
            /* If the recorded primitive is sc_print_error itself,
               this means the error is a result of a direct
               invocation, i.e. a highlevel error. */
            if (SYMBOL("raise-error") != p->var) { 
                symbol *s = object_to_symbol(p->var, &sc->m);
                if (s) _ex_printf(EX, " in `%s'", s->name); 
            }
        }
        _ex_printf(EX, ": ");
        sc_write_stderr(sc, e->tag); _ex_printf(EX, ": ");
        sc_write_stderr(sc, e->arg); _ex_printf(EX, "\n");
    }
    return VOID;
}


_ sc_symbol_to_string(sc *sc, _ sym) {
    symbol *s = CAST(symbol, sym);
    return _sc_make_string(sc, s->name);
}
_ sc_string_to_symbol(sc *sc, _ sym) {
    bytes *b = CAST(bytes, sym);
    return _ex_make_symbol(EX, b->bytes);
}

_ sc_bytes_length(sc *sc, _ ob) {
    return integer_to_object(CAST(bytes, ob)->size);
}

_ _sc_make_file_port(sc *sc, FILE *f, const char *name) {
    return _sc_make_aref(sc, (leaf_object *)port_file_new(f, name));
}
_ _sc_make_bytes_port(sc *sc, bytes *b) {
    return _sc_make_aref(sc, (leaf_object *)port_bytes_new(b));
}
_ sc_open_mode_file(sc *sc, _ path, _ mode) {
    bytes *b_path = CAST(bytes, path);
    bytes *b_mode = CAST(bytes, mode);
    FILE *f = fopen(b_path->bytes, b_mode->bytes);
    if (!f) ERROR("fopen", path);
    return _sc_make_file_port(sc, f, b_path->bytes);
}
_ sc_open_input_string(sc *sc, _ ob_str) {
    bytes *b = CAST(bytes, ob_str);
    bytes *copy_b = bytes_copy(b);
    return _sc_make_bytes_port(sc, copy_b);
}
_ sc_open_output_string(sc *sc) {
    bytes *b = bytes_new(20);
    b->size = 0;
    return _sc_make_bytes_port(sc, b);
}
_ sc_get_output_string(sc *sc, _ ob_port) {
    port *p = CAST(port, ob_port);
    bytes *b = port_get_bytes(p);
    if (!b) return TYPE_ERROR(ob_port);
    return _sc_make_aref(sc, b);
}

/* Returns a pair (input . output) of ports. */
_ sc_tcp_connect(sc *sc, _ host, _ port) {
    char *hostname  = CAST(cstring, host);
    int port_number = CAST_INTEGER(port);
    int fd;
    if (-1 == (fd = fd_socket(hostname, port_number, 0))) {
        ERROR("invalid", CONS(host, CONS(port, NIL)));
    }
    char name[20 + strlen(hostname)];
    sprintf(name, "I:%s:%d", hostname, port_number);
    _ in  = _sc_make_aref(sc, port_file_new(fdopen(fd, "r"), name));  name[0] = 'O';
    _ out = _sc_make_aref(sc, port_file_new(fdopen(fd, "w"), name));
    return CONS(in, out);
}

/* Returns a unix FILE DESCRIPTOR!  This can then be passed to
   accept_tcp to create an I/O port pair for each connection. */
_ sc_tcp_bind(sc *sc, _ host, _ port) {
    char *hostname  = CAST(cstring, host);
    int port_number = CAST_INTEGER(port);
    int fd;
    if (-1 == (fd = fd_socket(hostname, port_number, PORT_SOCKET_SERVER))) {
        ERROR("invalid", CONS(host, CONS(port, NIL)));
    }
    return integer_to_object(fd);
}

_ sc_tcp_accept(sc *sc, _ ob) {
    int server_fd = CAST_INTEGER(ob);
    int connection_fd = fd_accept(server_fd);
    if (-1 == connection_fd) ERROR("invalid-fd", ob);
    return CONS(_sc_make_aref(sc, port_file_new(fdopen(connection_fd, "r"), "I:tcp-accept")),
                _sc_make_aref(sc, port_file_new(fdopen(connection_fd, "w"), "O:tcp-accept")));
}

// Manually call finalizer, creating a defunct object.
_ sc_bang_finalize(sc *sc, _ ob) {
    aref *r = CAST(aref, ob);
    fin finalize = *(object_to_fin(r->fin));
    finalize(r->object, sc);
    r->fin = VOID;
    r->object = VOID;
    return VOID;
}

_ sc_close_port(sc *sc, _ ob) {
    port *p = CAST(port, ob);
    return sc_bang_finalize(sc, ob);
}
_ sc_flush_output_port(sc *sc, _ ob) {
    port_flush(CAST(port, ob));
    return VOID;
}


_ sc_system(sc *sc, _ ob) {
    char *cmd = object_to_cstring(ob, &sc->m);
    int rv = system(cmd);
    return integer_to_object(rv);
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


static inline _ _sc_call(sc *sc, void *p, int nargs, _ ra) {
    switch(nargs) {
    case 0: return ((ex_0)p)(EX);
    case 1: return ((ex_1)p)(EX, _CAR(ra));
    case 2: return ((ex_2)p)(EX, _CADR(ra), _CAR(ra));
    case 3: return ((ex_3)p)(EX, _CADDR(ra), _CADR(ra), _CAR(ra));
    default:
        return ERROR("prim", integer_to_object(nargs));
    }
}

/* Propagate environment during reduction. */
_ sc_close_args(sc *sc, _ lst, _ E) {
    if ((TRUE==IS_NULL(lst))) return NIL;
    else return CONS(REDEX(CAR(lst), E),
                     sc_close_args(sc, CDR(lst), E)); 
}

static inline void length_and_last(sc *sc, _ p, long* n, _*last) {
    *n = 0;
    while (TRUE==IS_PAIR(p)) {
        (*n)++; (*last) = CAR(p); p = CDR(p);
    }
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
    if (MT == k) ERROR("halt", value);

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
    if (TRUE == sc_is_k_apply(sc, k)) {
        /* If there are remaining closures to evaluate, push the value
           to the update value list and pop the next closure. */
        k_apply *kx = object_to_k_apply(k);
        if (TRUE==IS_PAIR(kx->todo)) {
            return STATE(CAR(kx->todo),
                         sc_make_k_apply(sc, kx->k.parent,
                                         CONS(value, kx->done),
                                         CDR(kx->todo)));
        }
        /* No more expressions to be reduced in the current k_apply:
           perform application. */
        else {
            _ rev_args = CONS(value, kx->done);
            _ fn=NIL;
            long n;
            length_and_last(sc, rev_args, &n, &fn);
            // n  == 1 + nb_args
            // fn == primitive | lambda | continuation

            /* Application of primitive function results in C call. */
            if (TRUE==IS_PRIM(fn)) {
                prim *p = object_to_prim(fn,&sc->m);
                sc->m.r.prim = p; // for debug
                if (prim_nargs(p) != (n-1)) {
                    return ERROR("nargs", fn);
                }
                /* Before entering primitive code, make GC restarts
                   illegal.  Code that allocates a large amount of
                   cells needs to re-enable restarts explicitly.  A
                   small number of cells are guaranteed to exist. */
                _ value = VALUE(VOID);
                _ state = STATE(value, kx->k.parent);
                EX->stateful_context = 1;
                _ rv = _sc_call(sc, prim_fn(p), n-1, rev_args);
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
        if (term_f == sc->s_lambda) {
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
            else body = CONS(sc->s_begin, body);
            _ l = sc_make_lambda(sc, formals, rest, body, env);
            return STATE(VALUE(l), k);
        }
        if (term_f == sc->s_quote) {
            if (NIL == term_args) goto syntax_error;
            return STATE(VALUE(CAR(term_args)), k);
        }
        if (term_f == sc->s_if) {
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
        if (term_f == sc->s_bang_set) {
            if (NIL == term_args) goto syntax_error;
            _ var = CAR(term_args);
            if (FALSE == IS_SYMBOL(var)) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ expr = CADR(term_args);
            return STATE(REDEX(expr, env),
                         sc_make_k_set(sc, k, var, env, sc_slot_toplevel));
        }
        if (term_f == sc->s_begin) {
            if (FALSE == IS_PAIR(term_args)) goto syntax_error;
            _ todo = sc_close_args(sc, term_args, env);
            pair *body = object_to_pair(todo);
            /* Don't create a contination frame if there's only a
               single expression. */
            if (NIL == body->cdr) return STATE(body->car, k);
            return STATE(body->car, sc_make_k_seq(sc, k, body->cdr));
        }                
        if (term_f == sc->s_letcc) {
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

    /* Extend the continuation with a new frame by collecting
       all (open) subterms, and binding them to the current
       environment. */
    _ closed_args = sc_close_args(sc, term_args, env);
    return STATE(REDEX(term_f, env),
                 sc_make_k_apply(sc, k, NIL, closed_args));
  syntax_error:
    return ERROR("syntax",term);
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
    ex_r save;

    memcpy(&save, &sc->m.r, sizeof(save));
    sc->m.r.prim = NULL; // means error comes from step() itself
    sc->m.prim_entries++;

    /* Allocate the error struct before the step() is entered to
       prevent GC restarts of imperative primitives. */ 
    if (FALSE == sc->error) {
        sc->error = sc_make_error(sc, VOID, VOID, VOID, VOID);
    }

    switch(exception = setjmp(sc->m.r.step)) {
        case EXCEPT_TRY:
            /* From here to the invocation of primitive code it is OK
               to perform a restart when a garbage collection occurs.
               We guarantee a minimum amount of free cells to
               primitive code, and will trigger collection here in
               case there is not enough. */
            EX->stateful_context = 0;
            if (gc_available(EX->gc) < 40) gc_collect(EX->gc);

            rv = _sc_step(sc, state);
            break;
        case EXCEPT_ABORT: 
        {
            error *e = object_to_error(sc->error);
            if (unlikely(NULL == e)) { TRAP(); }
            e->tag = sc->m.error_tag;
            e->arg = sc->m.error_arg;
            e->state = state;
            e->prim = const_to_object(sc->m.r.prim);
            sc->m.error_arg = NIL;
            sc->m.error_tag = NIL;
            rv = sc->error;
            sc->error = FALSE;
            break;
        }
        default:
            break;
    }

    sc->m.prim_entries--;
    memcpy(&sc->m.r, &save, sizeof(save));
    return rv;
}





/* GC: set continuation manually, since since the interpreter aborts
   and restarts the current step. */
_ sc_gc(sc* sc) {
    state *s = CAST(state, sc_global(sc, sc_slot_state));
    _ k = sc_k_parent(sc, s->continuation); // drop `gc' k_apply frame
    sc_bang_set_global(sc, sc_slot_state, 
                       STATE(VALUE(VOID), k)); // update state manually
    EX->stateful_context = 0;  // enable restarts
    gc_collect(sc->m.gc);      // collect will restart at sc->state
    return NIL; // not reached
}

_ sc_gc_used(sc *sc) {
    return integer_to_object(sc->m.gc->current_index);
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






_ sc_bang_abort_k(sc *sc, _ k) {
    return sc_bang_set_global(sc, sc_slot_abort_k, k);
}
_ sc_exit(sc *sc) {
    exit(0);
}

_ sc_make_bytes(sc *sc, _ ob) {
    int size = CAST_INTEGER(ob);
    if (size <= 0) return INVALID(ob);
    return _sc_make_bytes(sc, size);
}
_ sc_bytes_init(sc *sc, _ ob_bytes, _ ob_int) {
    bytes *b = CAST(bytes, ob_bytes);
    int fill = CAST_INTEGER(ob_int);
    memset(b->bytes, fill, b->size);
    return VOID;
}

/* --- SETUP & GC --- */


static void _sc_check_gc_size(sc *sc) {
    /* Check mem size. */
    gc *gc = EX->gc;
    long used = gc->current_index;
    long free = gc->slot_total - used;
    if (free < 100) {
        _ex_printf(EX, "growing GC\n");
        gc_alloc(gc, 100); // grow.
    }
}


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
    if (sc->m.top_entries) {
        _ex_printf(EX, "WARNING: multiple _sc_top() entries.\n");
        return NIL;
    }
    sc->m.top_entries++;
    sc_bang_set_global(sc, sc_slot_state, STATE(REDEX(expr,NIL),MT));
    for(;;) {
        if (setjmp(sc->m.top)){
            sc->m.prim_entries = 0;  // full tower unwind
            sc->m.r.prim = NULL;
        }
        // _sc_check_gc_size(sc);
        for(;;) {
            _ state;
            /* Run */
            do {
                state = sc_global(sc, sc_slot_state);      // get
                state = sc_eval_step(sc, state);           // update (functional)
                sc_bang_set_global(sc, sc_slot_state, state); // set
            }
            while (FALSE == sc_is_error(sc, state));
            
            /* Halt */
            error *e = object_to_error(state);
            if (e->tag == SYMBOL("halt")) {
                sc->m.top_entries--;
                return e->arg;
            }
            
            /* Abort */
            sc_bang_set_global(sc, sc_slot_state,
                               STATE(VALUE(state), 
                                     sc_global(sc, sc_slot_abort_k)));
        }
    }
}


static prim_def scheme_prims[] = scheme_table_init;
static prim_def ex_prims[] = ex_table_init;

void _sc_def_prims(sc *sc, prim_def *prims) {
    prim_def *prim;
    for (prim = prims; prim->name; prim++) {
        DEF(prim->name, prim->fn, prim->nargs);
    }
}



static void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // ex_trap(EX);
    // printf("gc_mark()\n");
    // sc_post(sc, sc->state);
    if (EX->stateful_context) {
        _ex_printf(EX, "FATAL: GC triggered in stateful context.");
        ex_trap(EX);
    }
    sc->global = gc_mark(sc->m.gc, sc->global);
    sc->error  = gc_mark(sc->m.gc, sc->error);

    if (fin) {
        /* We're given a finalizer continuation to aid us in aborting
           the C context that gave rise to the collection.  We use
           this to restart the current interpretation step saved in
           sc->state.  */
        fin(sc->m.gc);
        long used = sc->m.gc->current_index;
        long free = sc->m.gc->slot_total - used;
        GC_DEBUG { _ex_printf(EX, ";; gc %d:%d\n", (int)used, (int)free); }
        _ex_restart(EX);
    }
    else {
        /* No finalizer continuation means that this call is part of a
           gc_grow() operation, called from _sc_overflow(), which will
           handle restart.  We need to return to caller. */
        return;
    }
}
static _ _sc_make_prim(sc *sc, void *fn, long nargs, _ var) {
    prim *p = malloc(sizeof(*p));
    p->type = TYPES->prim_type;
    p->fn = fn;
    p->nargs = nargs;
    p->var = var;
    return const_to_object(p);
}
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs) {
    _ var = SYMBOL(str);
    sc_bang_def_toplevel(sc, var, _sc_make_prim(sc, fn, nargs, var));
}


void _sc_media_init(sc *sc);


#define SHIFT(n) {argv+=n;argc-=n;}
sc *_sc_new(int argc, char **argv) {
    sc *sc = calloc(1, sizeof(*sc));
    sc->m.top_entries = 0;
    sc->m.prim_entries = 0;

    char *bootfile = NULL;
    _ args = NIL;
    int verbose = 0;

    /* Read command line interpreter options options. */
    SHIFT(1); // skip program name
    while ((argc > 0) && ('-' == argv[0][0])) {
        if (!strcmp("--boot", argv[0])) { bootfile = argv[1]; SHIFT(2); }
        else if (!strcmp("--verbose", argv[0])) { SHIFT(1); verbose = 1; break; }
        else if (!strcmp("--", argv[0])) { SHIFT(1); break; }
        else {
            fprintf(stderr, "option `%s' not recognized\n", argv[0]);
            return NULL;
        }
    }

    /* Garbage collector. */
    sc->m.gc = gc_new(20000, sc, 
                      (gc_mark_roots)_sc_mark_roots,
                      (gc_overflow)_ex_overflow);
                    
    /* Atom classes. */
    base_types *types = NULL; // FIXME: configurable subclass?
    if (!types) types = malloc(sizeof(*(sc->m.p)));
    sc->m.p = types;
    // TYPES->ck_type = ck_type();
    TYPES->symbol_type = symbol_type();
    TYPES->prim_type = (void*)0xF001; // dummy class
    TYPES->port_type = port_type();
    TYPES->bytes_type = bytes_type();

    /* EX virtual methods */
    sc->m.port = (_ex_m_port)_sc_port;
    sc->m.write = (ex_m_write)sc_write_stderr;
    sc->m.make_string = (_ex_m_make_string)_sc_make_string;
    sc->m.make_qstring = (_ex_m_make_string)_sc_make_qstring;
    sc->m.make_pair = ex_cons;

    /* Data roots. */
    _ in  = _sc_make_file_port(sc, stdin,  "stdin");
    _ out = _sc_make_file_port(sc, stdout, "stdout");
    _ err = _sc_make_file_port(sc, stderr, "stderr");
    sc->global = gc_make_tagged(sc->m.gc, 
                                TAG_VECTOR,
                                7,
                                NIL,  // toplevel
                                NIL,  // macro
                                NIL,  // state
                                NIL,  // abort
                                in,
                                out,
                                err);
    sc->error = FALSE;

    /* Cached identifiers */
    sc->s_lambda   = SYMBOL("lambda");
    sc->s_if       = SYMBOL("if");
    sc->s_bang_set = SYMBOL("set!");
    sc->s_quote    = SYMBOL("quote");
    sc->s_begin    = SYMBOL("begin");
    sc->s_letcc    = SYMBOL("letcc");

    /* Primitive defs */
    _sc_def_prims(sc, ex_prims);
    _sc_def_prims(sc, scheme_prims);
    
#ifdef HAVE_MEDIA
    _sc_media_init(sc);
#endif
        

    /* Toplevel abort continuation */
    _ done = CONS(FIND(TOPLEVEL(),SYMBOL("print-error")),NIL);
    _ abort_k = sc_make_k_apply(sc, MT, done, NIL);

    sc_bang_abort_k(sc, abort_k);

    /* Pass command line arguments to scheme. */
    while ((argc > 0)) { args = CONS(STRING(argv[0]), args); SHIFT(1); }
    args = BANG_REVERSE(args);


    _ init;
    if (NIL == args) {
        init = CONS(SYMBOL("repl"), NIL);
    }
    else {
        init = CONS(SYMBOL("load"), CONS(CAR(args), NIL));
        args = CDR(args);
    }
    
    sc_bang_def_toplevel(sc, SYMBOL("args"), args);
    sc_bang_def_toplevel(sc, SYMBOL("init-script"), init);

    /* Highlevel bootstrap. */
    if (!bootfile) bootfile = getenv("PRIM_BOOT_SCM");
    if (!bootfile) bootfile = PRIM_HOME "boot.scm";
    if (verbose) _ex_printf(EX, "SC: %s\n", bootfile);
    _sc_top(sc, _ex_boot_load(EX, bootfile));
    return sc;
}

_ sc_read_no_gc(sc *sc, _ o) {
    port *p = CAST(port, o);
    return _ex_read(EX, p);
}

/* FIXME: currently this doesn't save the existing I/O ports (only for
   headless embedding). */

const char *_sc_repl_cstring(sc *sc, const char *commands) {
    bytes *bin = bytes_from_cstring(commands);
    bytes *bout = bytes_buffer_new(1000);
    _ in  = _sc_make_bytes_port(sc, bin);
    _ out = _sc_make_bytes_port(sc, bout);
    sc_bang_set_global(sc, sc_slot_input_port,  in);
    sc_bang_set_global(sc, sc_slot_output_port, out);
    sc_bang_set_global(sc, sc_slot_error_port,  out);
    _sc_top(sc, CONS(SYMBOL("repl-oneshot"), NIL));
    const char *output = cstring_from_bytes(bout);
    return output;
}


