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

#define TOP _pf_top(pf)


/* ERRORS */

_ _pf_abort(pf *pf) {
    longjmp(pf->m.r.step, PF_EX_ABORT);
}


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

#define PF_FREELIST_GC 1

/* Allocate a new linked list of pairs. */
#if PF_FREELIST_GC
_ _pf_alloc_freelist(pf *pf) {
    return CONS(VOID, NIL);
}
#else
_ _pf_alloc_freelist(pf *pf) {
    size_t nb = 100;
    pair *p = malloc(sizeof(pair) * nb);
    size_t i;
    for(i=0; i<nb; i++) {
        p[i].v.header = integer_to_object(2) | TAG_PAIR;
        p[i].car = VOID;
        p[i].cdr = vector_to_object(&p[i+1].v);
    }
    p[nb-1].cdr = NIL;
    return vector_to_object(&p[0].v);
}
#endif
/* Allocate/reuse cell. */
static inline void _pf_need_free(pf *pf) {
    if (unlikely(NIL == pf->free)) pf->free = _pf_alloc_freelist(pf);
}
static object _pf_cons(pf *pf, _ car, _ cdr) {
    _pf_need_free(pf);
    _ rv = pf->free;
    pf->free = CDR(pf->free);
    _CAR(rv) = car;
    _CDR(rv) = cdr;
    return rv;
}
/* Moving doesn't require refcount updates or copies. */
/* Moving cells. */
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
/* Moving variables in one atomic operation. */
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
        return _pf_cons(pf, 
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
static inline _ _pf_box(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_BOX, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
static inline _ _pf_lin(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_LIN, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
_ _pf_copy_to_graph(pf *pf, _ ob) {
    rc *x;
    pair *p;
    if ((x = object_to_rc(ob, &pf->m))) {
        /* Wrap all RC objects in a BOX struct */
        x->rc++;
        return _pf_lin(pf, ob);
    }
    else if ((p = object_to_pair(ob))) {
        /* Recursively copy the tree. */
        return ex_cons(&pf->m,
                       _pf_copy_to_graph(pf, p->car),
                       _pf_copy_to_graph(pf, p->cdr));
    }
    else return ob;
}
static inline void _pf_drop(pf *pf, _ *stack) {
    _pf_from_to(pf, stack, &pf->free);  // this catches underflow errors
    _ ob = MOVE(_CAR(pf->free), VOID);
    _pf_unlink(pf, ob);
}


void _pf_push(pf *pf, _ ob) {
    pf->ds = _pf_cons(pf, ob, pf->ds);
}
_ _pf_make_symbol(pf *pf, const char *str){
    return const_to_object(symbol_from_string(TYPES->symbol_type, str));
}



/* INTERPRETER */

/*
      http://zwizwa.be/-/libprim/20090816-120957

      SEQ   = (SUB : SUB)            ;; `:' is graph CONS
      SUB   = PRIM | QUOTE | SEQ
 
      RS    = NIL | (SUB . RS)       ;; `.' is linear CONS
*/
typedef void (*pf_prim)(pf*);

void pf_run(pf *pf) {
    seq *s;
    prim *p;
    quote *q;
    box *b;
    lin *l;

    /* Toplevel exceptions. */
    while (setjmp(pf->m.top));

  loop:
    /* Interpeter loop. */
    for(;;) {
        /* Unpack code sequence, push RS. */
        if ((s = object_to_seq(pf->ip))) {
            pf->rs = _pf_cons(pf, s->next, pf->rs);
            pf->ip = s->now;
        }
        /* Interpret primitive code or data and pop RS. */
        else {
            /* Update continuation. */
            _ ip = pf->ip;
            if (unlikely(NIL == pf->rs)) {
                pf->ip = pf->ip_halt;
            }
            else {
                pf->ip = CAR(pf->rs);
                _pf_drop(pf, &pf->rs);
            }
            /* Primitive */
            if ((p = object_to_prim(ip, &pf->m))) {
                pf_prim fn = (pf_prim)p->fn;
                pf->m.r.prim = p;
                switch(setjmp(pf->m.r.step)) {
                case 0:
                    fn(pf);
                    break;
                default:
                    _pf_push(pf, SYMBOL("unknown-exception"));
                case PF_EX_ABORT:
                    pf->ip = pf->ip_abort;
                }
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
                    if (unlikely(object_to_pair(q->object))) TRAP();
                    ob = q->object;
                }
                _pf_push(pf, ob);
            }
            /* The empty program. */
            else if (NOP == ip) {
            }
            /* Unknown non-seq. */
            else {
                TRAP();
            }
        }
    }

  halt:
    /* Return to caller. */
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

_ px_write(pf *pf, _ ob) {
    void *x;
    if (FALSE != ex_write(EX, ob)) {
        return VOID;
    }
    /* Ports are RC wrapped in PF.*/
    if ((x = object_to_port(ob, EX))) {
        return ex_write(EX, const_to_object(x));
    }
    else if ((x = object_to_seq(ob))) {
        return _ex_printf(EX, "#seq<%p>", x);
    }
    else {
        return _ex_printf(EX, "#object<%p>",(void*)ob);
    }
}

_ px_post(pf *pf, _ ob) {
    px_write(pf, ob);
    return _ex_printf(EX, "\n");
}


/* PRIMITIVES */

void pf_drop(pf *pf) {
    _pf_drop(pf, &pf->ds);
}
void pf_dup(pf *pf) {
    _pf_push(pf, _pf_link(pf, TOP));
}
void pf_dup_to_dict(pf *pf) {
    _ ob = _pf_copy_to_graph(pf, TOP);
    pf->dict = ex_cons(&pf->m, ob, pf->dict);
}
static object _box = 0;
void pf_box_test(pf *pf) {
    if (!_box) _box = _pf_box(pf, VOID);
    _pf_push(pf, _box);
}
void pf_bang(pf *pf) {
    aref *x = object_to_aref(TOP);
    x->object = CADR(pf->ds);
}

void pf_state(pf *pf) {
    _ex_printf(EX, "P: "); px_post(pf, pf->ds);
    _ex_printf(EX, "R: "); px_post(pf, pf->rs);
    _ex_printf(EX, "F: "); px_post(pf, pf->free);
    _ex_printf(EX, "D: "); px_post(pf, pf->dict);
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
    px_post(pf, TOP);
    pf_drop(pf);
}

// AUTOGEN
void pf_dup_write(pf *pf) { px_write(pf, TOP); }
void pf_dup_post(pf *pf)  { px_post(pf, TOP); }


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

_ px_skeleton_entry(pf *pf, _ code) { return CONS(CAR(code), FALSE); }
_ px_compile_defs(pf *pf, _ E_top, _ defs) {
    /* Create skeleton dictionary. */
    _ E_local = _ex_map1_prim(EX, (ex_1)px_skeleton_entry, defs);
    pair *penv  = CAST(pair, E_local);
    pair *pdefs = CAST(pair, defs);
    /* Translate to AST. */
    while (penv) { 
        _ entry = penv->car;
        _ src   = CDR(pdefs->car);
        _CDR(entry) = px_compile_program(pf, E_top, E_local, src);
        penv  = CAST(pair, penv->cdr);
        pdefs = CAST(pair, pdefs->cdr);
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

_ px_compile_program(pf *pf, _ E_top, _ E_local, _ src) {
    _ rv;
    _ *cursor = &rv;
    if (NIL == src) return NOP;
    /* Compile with proper tail calls. */
    for(;;) {
        _ compiled, datum = CAR(src);
        /* Quoted subprograms. */
        if (IS_LIST(datum)) {
            compiled = QUOTE(px_compile_program(pf, E_top, E_local, datum));
        }
        /* If possible, dereference.  In case we're compiling
           non-recursive code, this first pass will produce fully
           linked code. */
        else if (IS_SYMBOL(datum)) {
            _ val = FIND2(E_local, E_top, datum);
            if (FALSE != val) compiled = val;
            else compiled = datum;
        }
        /* Quote literal data. */
        else {
            // FIXME: distinguish between linear and graph data.
            _ val = datum;
            compiled = QUOTE(val);
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

/* GC + SETUP */

static void _pf_mark_roots(pf *pf, gc_finalize fin) {
    printf(";; gc\n");
    gc_mark(GC, pf->ds);
    gc_mark(GC, pf->rs);
    gc_mark(GC, pf->free);
    gc_mark(GC, pf->dict);
    if (fin) { fin(pf->m.gc); _ex_restart(EX); }
    else return;  // we're in gc_grow() -> return
}

static _ _pf_prim(pf* pf, pf_prim fn) {
    prim *p = malloc(sizeof(*p));
    p->type = TYPES->prim_type;
    p->fn = fn;
    p->nargs = 0;
    p->var = VOID;
    return const_to_object(p);
}

#define PRIM(fn)  _pf_prim(pf, fn)

pf* _pf_new(void) {
    pf *pf = malloc(sizeof(*pf));

    // Garbage collector.
    GC = gc_new(100000, pf, 
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
    pf->s_eval      = SYMBOL("eval");

    // Machine state.
    pf->rs = NIL;
    pf->ds = NIL;
    pf->free = NIL;
    pf->dict = NIL;
    pf->ip_halt  = PRIM((pf_prim)ex_trap);
    pf->ip_abort = PRIM(pf_print_error);
    pf->ip = 
        SEQ(PRIM(pf_dup_post),
            SEQ(PRIM(pf_output),
                SEQ(QUOTE(NUMBER(123)),
                    PRIM(pf_state))));

    // Stdout
    pf->output = _pf_make_port(pf, stdout, "stdout");

    return pf;
}


int main(int argc, char **argv) {
    pf *pf = _pf_new();
    long i = 0;

    pf_run(pf);

//    for(;;) {
/*         pf_output(pf); */
/*         // _pf_push(pf, integer_to_object(i++)); */
/*         // _pf_push(pf, integer_to_object(i++)); */
        
/*         pf_output(pf); */
/*         pf_dup(pf); */
/*         pf_stack(pf); */
        
/*         pf_state(pf); */
/*         pf_box_test(pf); */
/*         pf_bang(pf); */
/*         pf_state(pf); */

/*         pf_state(pf); */
/*         pf_box_test(pf); */

/*         pf_stack(pf); */
/*         pf_dup_to_dict(pf); */
/*         pf_state(pf); */

/*         //pf_dup_to_dict(pf); */
/*         //pf_state(pf); */

/*         // pf_stack(pf); */
/*         // pf_output(pf); */
/*         // pf_dup_post(pf); */
/*         // pf_drop(pf); */
/*         pf_trap(pf); */
//    }
    return 0;
}
