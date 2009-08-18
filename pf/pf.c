/*
  A linear, concatenative stack language.  A modularized rework of the
  ideas behind Packet Forth.

  The main memory model uses stacks and queues.  The objects are the
  same as used in the GC.  The word "packet" refers to leaf objects,
  which are atomic from the pov. of the scripting language.

  Functions with "pf_" prefix behave as scripting language
  primitives.  All other functions operating on the machine struct are
  prefixed with "_pf_".

 */

#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include "pf.h"
#include "pf.h_pf_prims"
#include "pf.h_px_prims"

#define TOP  _pf_top(pf)
#define _TOP _CAR(pf->ds)


/* TOOLS+DATA */

_ _pf_abort(pf *pf) {
    longjmp(pf->m.r.step, PF_EX_ABORT);
}
void pf_error_underflow(pf *pf) {
    _pf_push(pf, pf->s_underflow);
    _pf_abort(pf);
}
_ _pf_top(pf *pf) {
    if (unlikely (NIL == pf->ds)) pf_error_underflow(pf);
    return _CAR(pf->ds);
}
static inline void _pf_from_to(pf *pf, _ *from, _ *to) {
    if (unlikely(NIL == *from)) pf_error_underflow(pf);
    _ pair =  *from;
    *from = CDR(*from);
    _CDR(pair) = *to;
    *to = pair;
}
#define FROM_TO(a,b) _pf_from_to(pf, &(pf->a), &(pf->b))
_ static inline _move(_ *ob, _ filler) { _ o = *ob; *ob = filler; return o; }
#define MOVE(from,filler) _move(&from,filler)

/* Constant types are embedded in GC_CONST pointers, and can be
   identified by their first field (void*). */
DEF_ATOM(rc)
void *object_rc_struct(object ob, ex *m, void *type) {
    rc *x = object_to_rc(ob, m);
    if (!x) return NULL;
    void *x_type = *((void**)x->ctx);
    if (x_type != type) return NULL;
    return x->ctx;
}
_ _pf_make_rc(pf *pf, void *free, void *ctx) {
    rc *rc = malloc(sizeof(*rc));
    rc->type = TYPES->rc_type;
    rc->free = free;
    rc->ctx = ctx;
    rc->rc = 1;
    return const_to_object(rc);
}

/* RC types are const types wrapped in a RC struct. */
#define DEF_RC_TYPE(name)                                              \
    static inline name *object_to_##name(object ob, ex *m) {          \
        return (name*)object_rc_struct(ob,m,m->p->name##_type); }

DEF_RC_TYPE(port)

static _ _pf_make_port(pf *pf, FILE *f, const char *name) {
    return _pf_make_rc(pf, 
                       &(TYPES->port_type->free),
                       port_new(TYPES->port_type, stdout, name));
}
_ _pf_make_symbol(pf *pf, const char *str){
    return const_to_object(symbol_from_string(TYPES->symbol_type, str));
}
static void _exch(_*a, _*b) {
    _ tmp = *a;
    *a = *b;
    *b = tmp;
}
#define EXCH(a,b) _exch(&a, &b)





/* MEMORY MANAGEMENT

   The stack machine's inner data model consists of constants
   (permanent data), refcount managed abstact leaf objects and a
   linear tree of cons cells.

   The outer memory is a classic GC managed graph.  RC-managed data
   traversing the linear->graph boundary needs to be properly wrapped
   to synchronize the two memory managers.

   Graph data can be treated as constants in the linear memory, as the
   linear memory tree is part of the GC roots.
 */

/* Allocate a new linked list of pairs. */
_ _pf_alloc_freelist(pf *pf) {
    return CONS(VOID, NIL);
}
/* Allocate/reuse cell. */
static inline void _pf_need_free(pf *pf) {
    if (unlikely(NIL == pf->free)) pf->free = _pf_alloc_freelist(pf);
}
_ px_linear_cons(pf *pf, _ car, _ cdr) {
    _pf_need_free(pf);
    _ rv = pf->free;
    pf->free = _CDR(pf->free);
    _CAR(rv) = car;
    _CDR(rv) = cdr;
    return rv;
}
/* Unlink will RC manage objects, and move pairs to the freelist. */
static void _pf_unlink(pf* pf, _ ob);
static _ _pf_unlink_pop(pf *pf, _ lst) {
    _ ob = MOVE(_CAR(lst), VOID);
    _pf_unlink(pf, ob);
    _pf_from_to(pf, &lst, &pf->free);
    return lst;
}
static void _rc_unlink(rc *x) {
    if (!(x->rc--)) {
        x->free(x->ctx);
        free(x);
    }
}
static void _pf_unlink(pf* pf, _ ob) {
    rc *x;
  again:
    /* RC objects: dec RC and possibly free */
    if ((x = object_to_rc(ob, &pf->m))) {
        _rc_unlink(x);
    }
    /* Lists: recurse. */
    else if (object_to_pair(ob)) {
        ob = _pf_unlink_pop(pf, ob);
        goto again;
    }
}
/* Link will RC++ objects and recursively copy pair structures, using
   pairs from the freelist.. */
static _ _pf_link(pf *pf, _ ob) {
    rc *x = object_to_rc(ob, &pf->m);
    if (x) { 
        x->rc++;
        return ob;
    }
    else if (object_to_pair(ob)) {
        return px_linear_cons(pf, 
                        _pf_link(pf, CAR(ob)),
                        _pf_link(pf, CDR(ob)));
    }
    else return ob;
}
/* Whenever data is exported to the GC-managed side (graph memory or
   outer memory), CONS cells are copied and RC structs are wrapped.
   This performs a _copy_ instead of a an in-place move, which
   would interact badly with GC aborts due to mutation.

   There are 2 kinds of data structures hold linear values: a mutable
   box, and an immutable value.  BOX values are treated as graph
   values (constants), while LIN values are always unpacked into
   linear memory. */
static void _gc_unlink(_ ob, pf *pf) { _pf_unlink(pf, ob); }
static void *unlink_fin = _gc_unlink;
_ px_box(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_BOX, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
_ px_lin(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_LIN, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
_ _pf_copy_to_graph(pf *pf, _ ob) {
    rc *x;
    pair *p;
    /* Wrap all RC objects in a LIN struct */
    if ((x = object_to_rc(ob, &pf->m))) {
        x->rc++;
        return LIN(ob);
    }
    /* Recursively copy the tree. */
    else if ((p = object_to_pair(ob))) {
    
        return ex_cons(&pf->m,
                       _pf_copy_to_graph(pf, p->car),
                       _pf_copy_to_graph(pf, p->cdr));
    }
    else return ob;
}
_ _pf_copy_from_graph(pf *pf, _ ob) {
    pair *p;
    lin *l;
    /* Unwrap LIN objects. */
    if ((l = object_to_lin(ob))) { 
        return _pf_link(pf, l->object);
    }
    /* Recursive copy. */
    else if ((p = object_to_pair(ob))) {
        return px_linear_cons(pf, 
                        _pf_copy_from_graph(pf, p->car),
                        _pf_copy_from_graph(pf, p->cdr));
    }
    else return ob;
}
static inline void _pf_drop(pf *pf, _ *stack) {
    _pf_from_to(pf, stack, &pf->free);  // this catches underflow errors
    _ ob = MOVE(_CAR(pf->free), VOID);
    _pf_unlink(pf, ob);
}
void _pf_push(pf *pf, _ ob) {
    pf->ds = px_linear_cons(pf, ob, pf->ds);
}





/* INTERPRETER */

/*
      http://zwizwa.be/-/libprim/20090816-120957

      SEQ   = (SUB : SUB)            ;; `:' is graph CONS
      SUB   = PRIM | QUOTE | SEQ
 
      RS    = NIL | (SUB . RS)       ;; `.' is linear CONS
*/
typedef void (*pf_prim)(pf*);

void _pf_run(pf *pf) {
    seq *s;
    prim *p;
    quote *q;
    box *b;
    lin *l;

    /* Toplevel exceptions. */
    EX->top_entries++;
    while (setjmp(pf->m.top)) {
        pf->m.r.prim = NULL;
        pf->m.prim_entries = 0;
    }

  loop:
    /* Interpeter loop. */
    for(;;) {
        /* Unpack code sequence, push RS. */
        if ((s = object_to_seq(pf->ip))) {
            pf->rs = px_linear_cons(pf, s->next, pf->rs);
            pf->ip = s->now;
        }
        /* Interpret primitive code or data and pop RS. */
        else {
            /* Update continuation before executing primitive, so
               can modify the machine state. */
            _ ip = pf->ip;
            pair *rs = object_to_pair(pf->rs);
            if (unlikely(!rs)) pf->ip = HALT;
            else {
                pf->ip = rs->car;
                _pf_drop(pf, &pf->rs);
            }

            /* Primitive */
            if ((p = object_to_prim(ip, &pf->m))) {
                pf_prim fn = (pf_prim)p->fn;
                pf->m.r.prim = p;
                pf->m.prim_entries++;
                switch(setjmp(pf->m.r.step)) {
                case 0:
                    fn(pf);
                    break;
                default:
                    _pf_push(pf, SYMBOL("unknown-exception"));
                case PF_EX_ABORT:
                    // NONLINEAR
                    pf->m.error_tag = VOID;
                    pf->m.error_arg = VOID;
                    pf->ip = pf->ip_abort;
                }
                pf->m.prim_entries--;
            }
            /* Quoted object */
            else if ((q = object_to_quote(ip))) {
                _ ob;
                if ((l = object_to_lin(q->object))) {
                    /* Linear objects need to be copied. */
                    ob = _pf_link(pf, l->object);
                }
                else {
                    /* All other objects behave as constants to the
                       linear memory manager. */
                    if (unlikely((q->object != NIL) &&
                                 object_to_pair(q->object))) TRAP();
                    ob = q->object;
                }
                _pf_push(pf, ob);
            }
            /* The empty program. */
            else if (NOP == ip) {
            }
            /* Result of popping an empty continuation. */
            else if (HALT == ip) {
                goto halt;
            }
            /* Unknown non-seq. */
            else {
                _pf_push(pf, _pf_copy_from_graph(pf, ip));
                _pf_push(pf, SYMBOL("unknown-instruction"));
                pf->ip = pf->ip_abort;
            }
        }
    }

  halt:
    /* Return to caller. */
    EX->top_entries--;
    return;

}


/* EXPRESSIONS 

   It's simpler to factor out primitives as N -> 1 expressions, and
   then couple them (automatically?) to the parameter stack.  This
   uses the same naming convention as sc_ and ex_, namely

   px_  :  N x object -> object
   _px_ :  any other operation on *pf


*/


port *_pf_port(pf *pf) {
    return object_to_port(pf->output, &pf->m);
}

/* Print symbolic representation if possible. */
_ px_write_word(pf *pf, _ ob) {
    _ sym = UNFIND(pf->dict, ob);
    if (FALSE == sym) return px_write(pf, ob);
    else return px_write(pf, sym);
}

_ px_write(pf *pf, _ ob) {
    void *x;
    if (FALSE != _ex_write(EX, ob)) {
        return VOID;
    }
    /* Ports are RC wrapped in PF.*/
    if ((x = object_to_port(ob, EX))) {
        return _ex_write(EX, const_to_object(x));
    }
    else if ((x = object_to_box(ob))) {
        return _ex_write_vector(EX, "box", object_to_vector(ob));
    }
    /* SEQ and QUOTE are decompiled.  Use square brackets to
       distinguish from lists. */
    else if ((x = object_to_seq(ob))) {
        long max = 10;
        _ex_printf(EX, "[");
        for(;;) {
            seq *s = (seq*)x;
            px_write_word(pf, s->now);
            _ex_printf(EX, " ");
            ob = s->next;
            if (!(x = object_to_seq(ob))) {
                px_write_word(pf, s->next);
                _ex_printf(EX, "]");
                return VOID;
            }
            /* If the tail has a name, print that instead. */
            _ sym = UNFIND(pf->dict, ob);
            if (FALSE != sym) {
                px_write(pf, sym);
                return _ex_printf(EX, "]"); 
            }
            /* Prevent loops from generating too much output. */
            if (!(--max)) {
                return _ex_printf(EX, "...]"); 
            }
        }
    }
    else if ((x = object_to_quote(ob))) {
        quote *q = (quote*)x;
        /* Primitive */
        if ((object_to_prim(q->object, EX))) {
            _ex_printf(EX, "[");
            px_write_word(pf, q->object); 
            return _ex_printf(EX, "]");
        }
        /* Sequence */
        else if ((object_to_seq(q->object))) {
            return px_write(pf, q->object);
        }
        /* Datum */
        else {
            _ex_printf(EX, "'");
            return px_write(pf, q->object);
        }
    }
    else if (NOP == ob) {
        return _ex_printf(EX, "#nop");
    }
    else if (HALT == ob) {
        return _ex_printf(EX, "#halt");
    }
    else {
        return _ex_printf(EX, "#object<%p>",(void*)ob);
    }
}

/* COMPILER */

/* Compilation is factored into several steps.  The main distinction
   is again to perform the global side effects (update of the toplevel
   environement) _after_ all allocation has finished.

   These are the passes:

     1. Create skeleton environment.

     2. Translate s-expr -> SEQ | PRIM | QUOTE

     3. Resolve (remaining) undefined references.

     4. Patch toplevel environment.
*/


/* Convert definition list of (name . src) pairs to a compiled
   dictionary, using E_top for undefined references. */

_ px_skeleton_entry(pf *pf, _ code) { return CONS(CAR(code), VOID); }
_ px_compile_defs(pf *pf, _ E_top, _ defs) {
    /* Create skeleton dictionary. */
    _ E_local = _ex_map1_prim(EX, (ex_1)px_skeleton_entry, defs);
    _ penv  = E_local;
    _ pdefs = defs;
    /* Translate to AST. */
    while (NIL != penv) { 
        _ entry = CAR(penv);
        _ src   = CDAR(pdefs);
        _CDR(entry) = px_compile_program_env(pf, E_top, E_local, src);
        penv  = CDR(penv);
        pdefs = CDR(pdefs);
    }
    /* Resolve all references. */
    px_bang_resolve(pf, E_top, E_local);

    // FIXME:
    /* Check degenerate loops */
    /* Snap pointers. */
    return E_local;
}

/* Compile anonymous code to AST. */
_ px_quote(pf *pf, _ data)       { STRUCT(TAG_QUOTE, 1, data); }
_ px_seq(pf *pf, _ sub, _ next)  { STRUCT(TAG_SEQ, 2, sub, next); }

_ px_quote_datum(pf *pf, _ datum) {
    // FIXME: distinguish between linear and graph data.
    _ blessed = _pf_copy_from_graph(pf, datum);

    // Only LIN-wrap things that are necessary.
    if (object_to_pair(blessed)) blessed = LIN(blessed);

    return QUOTE(blessed);
}
_ px_compile_program_env(pf *pf, _ E_top, _ E_local, _ src) {
    _ rv;
    _ *cursor = &rv;
    if (NIL == src) return NOP;
    /* Compile with proper tail calls. */
    for(;;) {
        _ compiled, datum = CAR(src);
        /* Quoted empty program. */
        if (NIL == datum) {
            compiled = QUOTE(NOP);
        }
        else if (TRUE == IS_LIST(datum)) {
            /* Special Form. */
            _ tag = _CAR(datum);
            if (tag == pf->s_quote) {
                compiled = QUOTE_DATUM(_CADR(datum));
            }
            else if (tag == pf->s_var) {
                _ val = (NIL == _CDR(datum) ? VOID : _CADR(datum));
                compiled = QUOTE(BOX(_pf_copy_from_graph(pf, val)));
            }
            /* Quoted subprogram. */
            else {
                compiled = QUOTE_DATUM
                    (COMPILE_PROGRAM_ENV(E_top, E_local, datum));
            }
        }
        /* If possible, dereference.  In case we're compiling
           non-recursive code, this first pass will produce fully
           linked code. */
        else if (TRUE == IS_SYMBOL(datum)) {
            _ val = FIND2(E_local, E_top, datum);
            if (FALSE == val) ERROR("undefined", datum);
            if (VOID != val) compiled = val;
            else compiled = datum;
        }
        /* Quote literal data. */
        else {
            compiled = QUOTE_DATUM(datum);
        }
        /* Return if this was the last one. */
        if (NIL == CDR(src)) {
            *cursor = compiled;
            return rv;
        }
        /* Allocate sequence if there's more to come. */
        else {
            _ seq = SEQ(compiled, VOID);
            *cursor = seq;
            cursor = &(object_to_seq(seq)->next);
            src = CDR(src);
        }
    }
}
_ px_compile_program(pf *pf, _ src) {
    return px_compile_program_env(pf, NIL, pf->dict, src);
}

/* Walk the code, eliminating symbolic references where possible.
   Note: this only traverses the part of the code graph that's the
   result of a compilation. */
_ _px_resolve_sub(pf *pf, _ E_top, _ E_local,  _ *cursor);
_ _px_resolve_non_seq(pf *pf, _ E_top, _ E_local, _ *cursor) {
    _ sub = *cursor;
    quote *q;
    if ((q = object_to_quote(sub))) {
        return _px_resolve_sub(pf, E_top, E_local, &(q->object));
    }
    if (IS_SYMBOL(sub)) {
        _ val = FIND2(E_local, E_top, sub);
        if (FALSE != val) {
            *cursor = val;
            return ONE;
        }
    }
    return ZERO;
}
/* Resolve a flat subroutine without recursing into the call graph. */
_ _px_resolve_sub(pf *pf, _ E_top, _ E_local, _ *cursor) {
    _ derefs = ZERO;
    for(;;) {
        seq* s;
        _ sub = *cursor;
        /* Sequence, resolve NOW, then continue. */
        if ((s = object_to_seq(sub))) {
            _ dr = _px_resolve_non_seq(pf, E_top, E_local, &s->now);
            derefs = ADD(derefs, dr);
            cursor = &s->next;
        }
        /* Other: resolve and return. */
        else {
            _ dr = _px_resolve_non_seq(pf, E_top, E_local, cursor);
            return ADD(derefs, dr);
        }
    }
}
_ px_bang_resolve(pf *pf, _ E_top, _ E_local) {
    _ todo = E_local;
    _ derefs = ZERO;
    while (NIL != todo) {
        _ *cursor = &_CDAR(todo);
        derefs = ADD(derefs, _px_resolve_sub(pf, E_top, E_local, cursor));
        todo = CDR(todo);
    }
    return derefs;
}


_ px_define(pf *pf, _ defs) {
    /* Create an isolated environment. */
    _ E = px_compile_defs(pf, pf->dict, defs);

    /* Patch it into the global one. */
    while (NIL != E) {
        _ var = _CAAR(E);
        _ val = _CDAR(E);
        _ex_printf(EX, "define: "); POST(var);
        pf->dict = ENV_DEF(pf->dict, var, val);
        E = _CDR(E);
    }
    return VOID;
}


/* PRIMITIVES */

// stacks + boxes  (linear memory management)
void pf_drop(pf *pf) {
    _pf_drop(pf, &pf->ds);
}
void pf_dup(pf *pf) {
    _pf_push(pf, _pf_link(pf, TOP));
}
void pf_to_dict(pf *pf) {
    _ ob = _pf_copy_to_graph(pf, TOP);
    pf->dict = ex_cons(&pf->m, ob, pf->dict);
    _DROP();
}
void pf_fetch_(pf *pf) {
    aref *x = object_to_box(TOP);
    _TOP = _pf_link(pf, x->object);
}
void pf_bang_(pf *pf) {
    aref *x = object_to_box(TOP); _DROP();
    EXCH(_CAR(pf->ds), x->object); _DROP();
}
void pf_move_(pf *pf) {
    aref *x = object_to_box(TOP);
    _TOP = MOVE(x->object, VOID);
}
void pf_exchange(pf *pf) {
    aref *x = object_to_box(TOP);
    _DROP();
    EXCH(x->object, _TOP);
}




void pf_print_state(pf *pf) {
    _ex_printf(EX, "P: "); POST(pf->ds);
    _ex_printf(EX, "R: "); POST(pf->rs);
    _ex_printf(EX, "F: "); POST(pf->free);
}
void pf_print_dict(pf *pf) {
    _ E = pf->dict;
    while (NIL != E) {
        POST(CAR(E));
        E = CDR(E);
    }
}

void pf_output(pf *pf) {
    _pf_push(pf, _pf_link(pf, pf->output));
}
void pf_stack(pf *pf) {
    _pf_need_free(pf);
    FROM_TO(free, rs);
    _CAR(pf->rs) = MOVE(pf->ds, NIL);
    FROM_TO(rs, ds);
}
void pf_print_error(pf *pf) {
    if (NIL == pf->ds) _pf_push(pf, VOID);
    _ex_printf(EX, "ERROR: ");
    _POST();
}

/* Since we have a non-rentrant interpreter with mutable state, this
   is a bit less problematic than the EX/SC case. */
void pf_gc(pf *pf) {
    gc_collect(GC);
}

/* Primitives in terms of expressions.  Note that an upper case name
   like _XXX() is short for a stack word pf_xxx(pf *).  */
void pf_write(pf *pf)   { px_write(pf, TOP); _DROP(); }
void pf_post(pf *pf)    { POST(TOP); _DROP(); }
void pf_define(pf *pf)  { px_define(pf, TOP); _DROP(); }
void pf_trap(pf *pf)    { TRAP(); }
void pf_reverse(pf *pf) { _TOP = BANG_REVERSE(TOP); }
void pf_add1(pf *pf)    { _TOP = ADD1(TOP); }
void pf_compile(pf *pf) { _ v = COMPILE_PROGRAM(TOP); _DROP(); _pf_push(pf, v); }

void pf_run(pf *pf){ 
    _ v = TOP; _DROP();
    pf->rs = LINEAR_CONS(pf->ip, pf->rs); 
    pf->ip = v;
}
/* GC+SETUP */

#define GC_DEBUG _ex_printf(EX, ";; %d\n", (int)GC->current_index)
#define MARK(reg) pf->reg = gc_mark(GC, pf->reg)
static void _pf_mark_roots(pf *pf, gc_finalize fin) {
    printf(";; gc\n");
    MARK(ds);
    MARK(rs);
    MARK(free);
    MARK(output);
    MARK(ip);
    MARK(ip_abort);
    MARK(dict);
    if (fin) { 
        fin(GC); 
        long used = GC->current_index;
        long free = GC->slot_total - used;
        _ex_printf(EX, ";; gc %d:%d\n", (int)used, (int)free);
        _ex_restart(EX); 
    }
    else return;  // we're in gc_grow() -> return
}

static _ _pf_prim(pf* pf, pf_prim fn, _ name) {
    prim *p = malloc(sizeof(*p));
    p->type = TYPES->prim_type;
    p->fn = fn;
    p->nargs = 0;
    p->var = name;
    return const_to_object(p);
}

#define PRIM(fn)  _pf_prim(pf, fn)

static prim_def pf_prims[] = _pf_table_init;
static void _pf_def_prims(pf *pf, prim_def *prims) {
    prim_def *prim;
    for (prim = prims; prim->name; prim++) {
        _ var = SYMBOL(prim->name);
        pf->dict = ENV_DEF(pf->dict,
                           var,
                           _pf_prim(pf, prim->fn, var));
    }
}
static void _pf_def_all_prims(pf *pf) {
    _pf_def_prims(pf, pf_prims);
}

_ _pf_word(pf* pf, const char *str) {
    _ var = SYMBOL(str);
    _ rv = FIND(pf->dict, var);
    if (FALSE == rv) ERROR("undefined", var);
    return rv;
}

#define WORD(str) _pf_word(pf, str)

void _pf_load_lib(pf *pf);

pf* _pf_new(void) {
    pf *pf = malloc(sizeof(*pf));

    // Garbage collector.
    GC = gc_new(10000, pf, 
                (gc_mark_roots)_pf_mark_roots,
                (gc_overflow)_ex_overflow);

    // Leaf types.
    pf->m.p = malloc(sizeof(*(pf->m.p)));
    TYPES->ck_type = ck_class_new();
    TYPES->symbol_type = symbol_class_new(1000);
    TYPES->port_type = port_class_new();
    TYPES->prim_type = (void*)0xF001; 
    TYPES->rc_type = (void*)0xF002; 

    // Write delegate
    pf->m.write = (ex_write_method)px_write;
    pf->m.port  = (_ex_port_method)_pf_port;

    // Symbol cache
    pf->s_underflow = SYMBOL("underflow");
    pf->s_quote     = SYMBOL("quote");
    pf->s_var       = SYMBOL("var");

    // Machine state
    pf->ds = NIL;
    pf->free = NIL;
    pf->dict = NIL;
    pf->rs = NIL;
    pf->ip = HALT;
    pf->ip_abort = HALT;

    // Exceptions
    pf->m.top_entries = 0;
    pf->m.prim_entries = 0;

    // Stdout
    pf->output = _pf_make_port(pf, stdout, "stdout");

    // Primitives
    _pf_def_all_prims(pf);
    pf->ip_abort = FIND(pf->dict, SYMBOL("print-error"));

    // Highlevel bootstrap
    _pf_load_lib(pf);

    return pf;
}

/* Top level evaluator.  This takes a (read-only) s-expression,
   compiles it to code (this performs allocation from GC pool -- top
   eval is not linear), and executes this code until machine halt.  */
void _pf_top_interpret_list(pf *pf, _ expr){
    pf->ip = px_compile_program(pf, expr);
    _pf_run(pf);
}
/* Find and run.  This is linear if the referenced code is. */
void _pf_top_interpret_symbol(pf *pf, _ sym) {
    pf->ip = FIND(pf->dict, sym);
    _pf_run(pf);
}
void _pf_top_command(pf *pf, const char *str) {
    _pf_top_interpret_symbol(pf, SYMBOL(str));
}

int main(int argc, char **argv) {
    pf *pf = _pf_new();
    _pf_run(pf);
    return 0;
}
