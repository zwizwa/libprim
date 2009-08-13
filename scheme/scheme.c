#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "scheme.h"

// generated
#include "scheme.h_"

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
    long i = CAST_INTEGER(o);
    if (i) return FALSE;
    return TRUE;
}
_ sc_add1(sc *sc, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i + 1);
}
_ sc_sub1(sc *sc, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i - 1);
}

/* Symbols are encoded as GC_ATOM. */

/* Predicates for primitive objects are derived from their
   object_to_pointer cast: if it returns NULL, the type isn't
   correct. */
#define OBJECT_PREDICATE(cast) \
    {if (cast(o, sc)) return TRUE; else return FALSE;}
_ sc_is_symbol(sc *sc, _ o) { OBJECT_PREDICATE(object_to_symbol); }
_ sc_is_prim(sc *sc, _ o)   { OBJECT_PREDICATE(object_to_prim); }
_ sc_is_ck(sc *sc, _ o)     { OBJECT_PREDICATE(object_to_ck); }
_ sc_is_port(sc *sc, _ o)   { OBJECT_PREDICATE(object_to_port); }
_ sc_is_bytes(sc *sc, _ o)  { OBJECT_PREDICATE(object_to_bytes); }


/* The empty list is the NULL pointer */
_ sc_is_null(sc *sc, _ o) {
    if (!o) return TRUE; else return FALSE;
}
/* Pairs and lambdas are tagged vectors. */
static _ vector_type(_ o, long flags) {
    vector *v;
    if ((v = object_to_vector(o)) &&
        (flags == vector_to_flags(v))) { return TRUE; }
    return FALSE;
}


// vector tags for interpreter data types
#define TAG_VECTOR    VECTOR_TAG(0)

#define TAG_PAIR      VECTOR_TAG(1)
#define TAG_LAMBDA    VECTOR_TAG(2)
#define TAG_STATE     VECTOR_TAG(3)
#define TAG_VALUE     VECTOR_TAG(4)
#define TAG_REDEX     VECTOR_TAG(5)
#define TAG_ERROR     VECTOR_TAG(6)
#define TAG_AREF      VECTOR_TAG(7)

#define TAG_K_IF      VECTOR_TAG(8)
#define TAG_K_SET     VECTOR_TAG(9)
#define TAG_K_APPLY   VECTOR_TAG(10)
#define TAG_K_SEQ     VECTOR_TAG(11)
#define TAG_K_MACRO   VECTOR_TAG(12)
#define TAG_K_IGNORE  VECTOR_TAG(13)
// reserved 14 15
static inline long flags_is_k(long flag) {
    return flag & VECTOR_TAG(8);
}

/* Predicates */
_ sc_is_vector(sc *sc, _ o)  { return vector_type(o, TAG_VECTOR); }
_ sc_is_pair(sc *sc, _ o)    { return vector_type(o, TAG_PAIR); }
_ sc_is_lambda(sc *sc, _ o)  { return vector_type(o, TAG_LAMBDA); }
_ sc_is_state(sc *sc, _ o)   { return vector_type(o, TAG_STATE); }
_ sc_is_redex(sc *sc, _ o)   { return vector_type(o, TAG_REDEX); }
_ sc_is_value(sc *sc, _ o)   { return vector_type(o, TAG_VALUE); }
_ sc_is_error(sc *sc, _ o)   { return vector_type(o, TAG_ERROR); }
_ sc_is_aref(sc *sc, _ o)    { return vector_type(o, TAG_AREF); }

_ sc_is_k_if(sc *sc, _ o)    { return vector_type(o, TAG_K_IF); }
_ sc_is_k_apply(sc *sc, _ o) { return vector_type(o, TAG_K_APPLY); }
_ sc_is_k_seq(sc *sc, _ o)   { return vector_type(o, TAG_K_SEQ); }
_ sc_is_k_set(sc *sc, _ o)   { return vector_type(o, TAG_K_SET); }
_ sc_is_k_macro(sc *sc, _ o) { return vector_type(o, TAG_K_MACRO); }

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


_ sc_cons(sc *sc, _ car, _ cdr)                   {STRUCT(TAG_PAIR,    2, car,cdr);}
_ sc_make_state(sc *sc, _ C, _ K)                 {STRUCT(TAG_STATE,   2, C,K);}
_ sc_make_lambda(sc *sc, _ F, _ R, _ S, _ E, _ M) {STRUCT(TAG_LAMBDA,  5, F,R,S,E,M);}
_ sc_make_error(sc *sc, _ T, _ A, _ K, _ X)       {STRUCT(TAG_ERROR,   4, T,A,K,X);}
_ sc_make_redex(sc *sc, _ D, _ E, _ M)            {STRUCT(TAG_REDEX,   3, D,E,M);}
_ sc_make_value(sc *sc, _ D)                      {STRUCT(TAG_VALUE,   1, D);}
_ sc_make_aref(sc *sc, _ F, _ O)                  {STRUCT(TAG_AREF,    2, F,O);}


// 'P' is in slot 0
// continuations are created with an empty mark list
_ sc_make_k_apply(sc *sc, _ P, _ D, _ T)     {STRUCT(TAG_K_APPLY,  4, P,NIL,D,T);}
_ sc_make_k_if(sc *sc, _ P, _ Y, _ N)        {STRUCT(TAG_K_IF,     4, P,NIL,Y,N);}
_ sc_make_k_set(sc *sc, _ P, _ V, _ E, _ Et) {STRUCT(TAG_K_SET,    5, P,NIL,V,E,Et);}
_ sc_make_k_seq(sc *sc, _ P, _ T)            {STRUCT(TAG_K_SEQ,    3, P,NIL,T);}
_ sc_make_k_macro(sc *sc, _ P, _ E, _ M)     {STRUCT(TAG_K_MACRO,  4, P,NIL,E,M);}


_ _sc_make_aref(sc *sc, void *fin, void *ptr) {
    return sc_make_aref(sc, fin_to_object(fin), const_to_object(ptr));
}

_ _sc_make_symbol(sc *sc, const char *str) {
    return const_to_object((void*)(string_to_symbol(sc->symbol_type, str)));
}
_ _sc_make_string(sc *sc, const char *str) {
    return _sc_make_aref(sc, sc->bytes_type,
                         bytes_from_cstring(sc->bytes_type, str));
}


_ sc_car(sc *sc, _ o)  { pair *p = CAST(pair, o); return p->car; }
_ sc_cdr(sc *sc, _ o)  { pair *p = CAST(pair, o); return p->cdr; }
_ sc_cadr(sc *sc, _ o) { pair *p = CAST(pair, sc_cdr(sc, o)); return p->car; }

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
    _sc_printf(sc, "ERROR: attempt to abort primitive outside of the main loop.\n");
    sc_trap(sc);
    exit(1);
}
_ sc_type_error(sc *sc, _ arg_o) {
    return sc_error(sc, SYMBOL("type"), arg_o);
}
_ sc_make_vector(sc *sc, _ slots, _ init) {
    long i,n = CAST_INTEGER(slots);
    vector *v = gc_alloc(sc->gc, n);
    for(i=0; i<n; i++) v->slot[i] = init;
    return vector_to_object(v);
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

// Take n elements from the head of a list and place them in a vector.
_ sc_take_vector(sc *sc, _ n, _ in_lst) {
    _ lst = in_lst;
    long slots = CAST_INTEGER(n);
    vector *v = gc_alloc(sc->gc, slots);
    long i;
    for(i=0; i<slots; i++){
        if (FALSE == sc_is_pair(sc, lst)) return TYPE_ERROR(in_lst);
        pair *p = object_to_pair(lst);
        v->slot[i] = p->car;
        lst = p->cdr;
    }
    return vector_to_object(v);
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
    long index = CAST_INTEGER(n);
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
    // _sc_printf(sc, "DEF %s: ",s->name); sc_post(sc, val);
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
_ sc_newline(sc *sc, _ out) { port_printf(CAST(port, out), "\n"); return VOID; }

static _ write_vector(sc *sc, char *type, _ o, _ output_port) {
    port *p = CAST(port, output_port);
    vector *v = object_to_vector(o);
    long i,n = vector_size(v);
    port_printf(p, "#%s(", type);
    for(i=0;i<n;i++){
        sc_write(sc, v->slot[i], output_port);
        if (i != n-1) port_printf(p, " ");
    }
    port_printf(p, ")");
    return VOID;
}
_ sc_write(sc *sc,  _ o, _ out) {
    port *p = CAST(port, out);
    if (TRUE  == o) { port_printf(p, "#t"); return VOID; }
    if (FALSE == o) { port_printf(p, "#f"); return VOID; }
    if (TRUE == sc_is_integer(sc, o)) {
        port_printf(p, "%ld", object_to_integer(o));
        return VOID;
    }
    if (VOID  == o) { port_printf(p, "#<void>"); return VOID; }
    if(TRUE == sc_is_null(sc, o)) {
        port_printf(p, "()");
        return VOID;
    }
    if (TRUE == sc_is_pair(sc, o)) {
        port_printf(p, "(");
        for(;;) {
            sc_write(sc, CAR(o), out);
            o = CDR(o);
            if (TRUE == sc_is_null(sc, o)) {
                port_printf(p, ")");
                return VOID;
            }
            if (FALSE == sc_is_pair(sc, o)) {
                port_printf(p, " . ");
                sc_write(sc, o, out);
                port_printf(p, ")");
                return VOID;
            }
            port_printf(p, " ");
        }
    }
    if (TRUE == sc_is_vector(sc, o))  return write_vector(sc, "", o, out);
    if (TRUE == sc_is_symbol(sc, o)) {
        port_printf(p, "%s", object_to_symbol(o,sc)->name);
        return VOID;
    }
    if (TRUE == sc_is_prim(sc, o)) {
        prim *pr = object_to_prim(o,sc);
        port_printf(p, "#prim<%p:%ld>", (void*)(pr->fn),pr->nargs);
        return VOID;
    }
    void *x;
    if ((x = object_to_fin(o))) { 
        port_printf(p, "#fin<%p:%p>", x, *((void**)x)); return VOID; 
    }
    if ((x = object_to_const(o))) { port_printf(p, "#data<%p>",x); return VOID; }

    if (TRUE == sc_is_bytes(sc, o)) {
        bytes_write_string(object_to_bytes(o, sc), p->stream);
        return VOID;
    }
    if (TRUE == sc_is_state(sc, o))   return write_vector(sc, "state", o, out);
    if (TRUE == sc_is_lambda(sc, o))  return write_vector(sc, "lambda", o, out);
    if (TRUE == sc_is_redex(sc, o))   return write_vector(sc, "redex", o, out);
    if (TRUE == sc_is_value(sc, o))   return write_vector(sc, "value", o, out);
    if (TRUE == sc_is_error(sc, o))   return write_vector(sc, "error", o, out);
    if (TRUE == sc_is_aref(sc, o))    return write_vector(sc, "aref", o, out);

    if (TRUE == sc_is_k_apply(sc, o)) return write_vector(sc, "k_apply", o, out);
    if (TRUE == sc_is_k_if(sc, o))    return write_vector(sc, "k_if", o, out);
    if (TRUE == sc_is_k_seq(sc, o))   return write_vector(sc, "k_seq", o, out);
    if (TRUE == sc_is_k_set(sc, o))   return write_vector(sc, "k_set", o, out);
    if (TRUE == sc_is_k_macro(sc, o)) return write_vector(sc, "k_macro", o, out);
    if (MT   == o) { port_printf(p, "#k_mt"); return VOID; }

    port_printf(p, "#object<%p>",(void*)o);
    return VOID;
}

/* Use current output.  Until params work, this is the debug port. */
_ _sc_printf(sc *sc, char *fmt, ...) {
    int rv;
    port *p = CAST(port, sc_global(sc, sc_slot_debug_port));
    va_list ap; va_start(ap, fmt);
    rv = port_vprintf(p, fmt, ap);
    va_end(ap);
    return rv;
}
_ sc_post(sc* sc, _ o) {
    _ dbg = sc_global(sc, sc_slot_debug_port);
    if (VOID != o) {
        sc_write(sc, o, dbg);
        _sc_printf(sc, "\n");
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
    _ dbg = sc_global(sc, sc_slot_debug_port);
    if (TRUE == sc_is_error(sc, err)) {
        error *e = object_to_error(err);
        _sc_printf(sc, "ERROR");
        if (TRUE == sc_is_prim(sc, e->prim)) {
            prim *p = object_to_prim(e->prim, sc);
            symbol *s = object_to_symbol(p->var, sc);
            if (s) _sc_printf(sc, " in `%s'", s->name); 
        }
        _sc_printf(sc, ": ");
        sc_write(sc, e->tag, dbg); _sc_printf(sc, ": ");
        sc_write(sc, e->arg, dbg); _sc_printf(sc, "\n");
    }
    return VOID;
}

_ sc_mt(sc *sc)    { return MT; }
_ sc_true(sc *sc)  { return TRUE; }
_ sc_false(sc *sc) { return FALSE; }
_ sc_void(sc *sc)  { return VOID; }

_ sc_symbol_to_string(sc *sc, _ sym) {
    symbol *s = CAST(symbol, sym);
    return _sc_make_string(sc, s->name);
}
_ sc_string_to_symbol(sc *sc, _ sym) {
    bytes *b = CAST(bytes, sym);
    return _sc_make_symbol(sc, b->bytes);
}
_ sc_list_clone(sc *sc, _ lst) {
    if (NIL == lst) return lst;
    _ res = CONS(VOID, NIL);
    pair *in,*out;
    out = object_to_pair(res);
    for(;;) {
        in  = CAST(pair, lst);
        if (NIL == in->cdr) return res;
        out->cdr = CONS(VOID,NIL);
        out = object_to_pair(out->cdr);
        lst = in->cdr;
    }
}

#define NARGS_ERROR(fn) ERROR("nargs", fn)

/* Primitive map to make some interpret code a bit easier. */
static _ _sc_map1_prim(sc *sc, sc_1 fn, _ l_in) {
    _ res = sc_list_clone(sc, l_in);
    _ l_out = res;
    pair *in, *out;
    for(;;) {
        in  = object_to_pair(l_in);
        out = object_to_pair(l_out);
        if (!in) return res;
        out->car = fn(sc, in->car);
        l_in  = in->cdr;
        l_out = out->cdr;
    }
}
_ sc_map1_prim(sc *sc, _ fn, _ l_in) {
    prim *p = CAST(prim, fn);
    if (1 != p->nargs) NARGS_ERROR(fn);
    return _sc_map1_prim(sc, (sc_1)(p->fn), l_in);
}
static _ _sc_map2_prim(sc *sc, sc_2 fn, _ l_in1, _ l_in2) {
    _ res = sc_list_clone(sc, l_in1);
    _ l_out = res;
    pair *in1, *in2, *out;
    for(;;) {
        in1 = object_to_pair(l_in1);
        in2 = object_to_pair(l_in2);
        out = object_to_pair(l_out);
        if ((!in1) || (!in2)) return res;
        out->car = fn(sc, in1->car, in2->car);
        l_in1 = in1->cdr;
        l_in2 = in2->cdr;
        l_out = out->cdr;
    }
}
_ sc_map2_prim(sc *sc, _ fn, _ l_in1, _ l_in2) {
    prim *p = CAST(prim, fn);
    if (2 != p->nargs) NARGS_ERROR(fn);
    return _sc_map2_prim(sc, (sc_2)(p->fn), l_in1, l_in2);
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

/* Propagate environment during reduction. */
_ sc_close_args(sc *sc, _ lst, _ E, _ M) {
    if ((TRUE==sc_is_null(sc, lst))) return NIL;
    else return CONS(REDEX(CAR(lst), E, M),
                     sc_close_args(sc, CDR(lst), E, M)); 
}

static inline void length_and_last(sc *sc, _ p, long* n, _*last) {
    *n = 0;
    while (TRUE==sc_is_pair(sc,p)) {
        (*n)++; (*last) = CAR(p); p = CDR(p);
    }
}

_ _sc_step_value(sc *sc, _ v, _ k) {

    /* Look at the continuation to determine what to do with the value. 
       
       - empty continuation -> halt
       - argument evaluation -> eval next, or apply
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
        return STATE(REDEX(value,kx->env,kx->menv), kx->k.parent);
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
            _ fn=NIL;
            long n;
            length_and_last(sc, rev_args, &n, &fn);
            // n  == 1 + nb_args
            // fn == primitive | lambda | continuation

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
                _ fn_menv = l->menv;

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
                return STATE(REDEX(l->term, fn_env, fn_menv),  // close term
                             kx->k.parent);                    // drop frame
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
    _ env, menv; // E
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
        menv = r->menv;
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
            if (NIL == term_args) goto syntax_error;
            _ argspec = CAR(term_args);
            _ named;
            _ rest;
            _sc_length_rest(sc, argspec, &named, &rest);
            if ((NIL   != rest) &&
                (FALSE == sc_is_symbol(sc,rest))) {
                goto syntax_error;
            }
            _ formals = sc_take_vector(sc, named, argspec);
            /* Implement the expression sequence in a `lambda'
               expression as a `begin' sequencing form. */
            _ body = CDR(term_args);
            if (NIL == CDR(body)) body = CAR(body);
            else body = CONS(sc->s_begin, body);
            _ l = sc_make_lambda(sc, formals, rest, body, env, menv);
            return STATE(VALUE(l), k);
        }
        if (term_f == sc->s_quote) {
            if (NIL == term_args) goto syntax_error;
            return STATE(VALUE(CAR(term_args)), k);
        }
        if (term_f == sc->s_if) {
            if (NIL == term_args) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ cond = REDEX(CAR(term_args),env,menv);
            _ yes  = REDEX(CADR(term_args),env,menv);
            _ no   = 
                (NIL == CDDR(term_args)) ? 
                VALUE(VOID) :
                REDEX(CADDR(term_args),env,menv);
            return STATE(cond, sc_make_k_if(sc, k, yes,no));
                                              
        }
        if (term_f == sc->s_bang_set) {
            if (NIL == term_args) goto syntax_error;
            _ var = CAR(term_args);
            if (FALSE == sc_is_symbol(sc, var)) goto syntax_error;
            if (NIL == CDR(term_args)) goto syntax_error;
            _ expr = CADR(term_args);
            return STATE(REDEX(expr, env, menv),
                         sc_make_k_set(sc, k, var, env, sc_slot_toplevel));
        }
        if (term_f == sc->s_begin) {
            if (FALSE == sc_is_pair(sc, term_args)) goto syntax_error;
            _ todo = sc_close_args(sc, term_args, env, menv);
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
            _ cl  = REDEX(CADR(term_args),env,menv);
            return STATE(cl, k);
        }
        _ macro;
        if (FALSE != (macro = sc_find(sc, sc_toplevel_macro(sc), term_f))) {
            /* Macro continuation is based on a completed
               k_apply frame that will trigger the fn
               application, linked to a k_macro frame that
               will steer the result back to the AST
               reducer. */
            _ k_m = sc_make_k_macro(sc, k, env, menv);
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
    _ closed_args = sc_close_args(sc, term_args, env, menv);
    return STATE(REDEX(term_f, env, menv),
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
    _sc_printf(sc, "ERROR: attempt restart outside of the main loop.\n");
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
    return sc_make_k_seq(sc, k, CONS(REDEX(expr, NIL, NIL),NIL));
}




/* Invoke a C continuation 
   
   This is a data barrier: C tasks can never have access to Scheme
   objects.  All data passes through a converter in the ck_manager.
*/

static _ test_ck(ck_class *m, _ o) {
    printf("1: test_ck()\n"); o = (object)ck_yield(m, (void*)o);
    printf("2: test_ck()\n"); o = (object)ck_yield(m, (void*)o);
    printf("3: test_ck()\n");
    return o;
}

_ sc_with_ck(sc *sc, _ in_ref, _ value) {
    ck *task = NULL;
    ck_start fn = NULL;
    if (!(task = object_to_ck(in_ref, sc))) {
        fn = (ck_start)test_ck;
    }
    ck *in_task = task;

    // alloc before call
    _ ref = sc_make_aref(sc, NIL, NIL);
    _ stream = CONS(NIL, ref);  
    
    ck_invoke(sc->ck_type, fn, &task, (void**)&value);

    if (!task) return value;
    else {
        pair *p = object_to_pair(stream);
        p->car = value;
        if (in_task == task) {
            p->cdr  = in_ref;
        }
        else {
            aref *r = object_to_aref(ref);
            r->atom = const_to_object(task);
            r->fin  = fin_to_object((fin *)sc->ck_type);
        }
        return stream;
    }
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
        _sc_printf(sc, "WARNING: multiple _sc_top() entries.\n");
        return NIL;
    }
    sc->top_entries++;
    sc_bang_set_global(sc, sc_slot_state, STATE(REDEX(expr,NIL,NIL),MT));
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

#define USE_TABLE_PRIMS 1
#if USE_TABLE_PRIMS
typedef struct {
    const char *name;
    void *fn;
    int nargs;
} prim_def;
static prim_def prims[] = prims_init;
#endif

void _sc_overflow(sc *sc, long extra) {
    /* At this point, the heap is compacted, but the requested
       allocation doesn't fit.  We need to grow.  Take at least the
       requested size + grow by a fraction of the total heap. */
    long request = extra + (sc->gc->slot_total/4);
    _sc_printf(sc, ";; gc-overflow %ld:%ld\n", extra, request);
    gc_grow(sc->gc, request);
    _sc_restart(sc);
}

static void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // sc_trap(sc);
    // printf("gc_mark()\n");
    // sc_post(sc, sc->state);
    sc->global = gc_mark(sc->gc, sc->global);

    if (fin) {
        /* We're given a finalizer continuation to aid us in aborting
           the C context that gave rise to the collection.  We use
           this to restart the current interpretation step saved in
           sc->state.  */
        fin(sc->gc);
        long used = sc->gc->current_index;
        long free = sc->gc->slot_total - used;
        _sc_printf(sc, ";; gc %d:%d\n", (int)used, (int)free);
        _sc_restart(sc);
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
    p->type = sc->prim_type;
    p->fn = fn;
    p->nargs = nargs;
    p->var = var;
    return const_to_object(p);
}
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs) {
    _ var = SYMBOL(str);
    sc_bang_def_toplevel(sc, var, _sc_make_prim(sc, fn, nargs, var));
}
void _sc_load_lib(sc* sc);

sc *_sc_new(void) {
    sc *sc = malloc(sizeof(*sc));
    sc->top_entries = 0;
    sc->step_entries = 0;

    /* Garbage collector. */
    sc->gc = gc_new(10000, sc, 
                    (gc_mark_roots)_sc_mark_roots,
                    (gc_overflow)_sc_overflow);
                    

    /* Atom classes. */
    sc->ck_type = ck_class_new();
    sc->symbol_type = symbol_class_new(1000);
    sc->prim_type = (void*)(123); // FIXME: dummy class
    sc->port_type = port_class_new();
    _ out = _sc_make_aref(sc, sc->port_type, 
                          port_new(sc->port_type, stderr));

    sc->global = gc_make(sc->gc, 5,
                         NIL,  // toplevel
                         NIL,  // macro
                         NIL,  // state
                         NIL,  // abort
                         out); // debug port

    /* Cached identifiers */
    sc->s_lambda   = SYMBOL("lambda");
    sc->s_if       = SYMBOL("if");
    sc->s_bang_set = SYMBOL("set!");
    sc->s_quote    = SYMBOL("quote");
    sc->s_begin    = SYMBOL("begin");
    sc->s_letcc    = SYMBOL("letcc");

    /* Primitive defs */
#if USE_TABLE_PRIMS
    prim_def *prim;
    for (prim = prims; prim->name; prim++) {
        DEF(prim->name, prim->fn, prim->nargs);
    }
#else
    // apparently this gives smaller code
    _sc_def_prims(sc); // defined in scheme.h_
#endif

    /* Toplevel abort continuation */
    _ done = CONS(sc_find_toplevel(sc, SYMBOL("fatal")),NIL);
    _ abort_k = sc_make_k_apply(sc, MT, done, NIL);
    sc_bang_set_global(sc, sc_slot_abort_k, abort_k);

    /* Highlevel bootstrap */
    _sc_load_lib(sc);
    return sc;
}


