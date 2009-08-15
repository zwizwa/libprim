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
#include "../ex/ex_prims.h_"

#define TOP ARG0
#define ARG0 CAR(pf->ds) 
#define ARG1 CADR(pf->ds)

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
    return ex_cons(&pf->m, VOID, NIL);
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
    CAR(rv) = car;
    CDR(rv) = cdr;
    return rv;
}
/* Moving doesn't require refcount updates or copies. */
/* Moving cells. */
void pf_error_underflow(pf *pf) {
    _pf_push(pf, pf->s_underflow);
    _pf_abort(pf);
}
static inline void _pf_from_to(pf *pf, _ *from, _ *to) {
    if (unlikely(NIL == *from)) pf_error_underflow(pf);
    _ pair =  *from;
    *from = CDR(*from);
    CDR(pair) = *to;
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
    _ ob = MOVE(CAR(lst), VOID);
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
static inline void _pf_dropstack(pf *pf, _ *stack) {
    _pf_from_to(pf, stack, &pf->free);  // this catches underflow errors
    _ ob = MOVE(CAR(pf->free), VOID);
    _pf_unlink(pf, ob);
}
#define DROP(stack) _pf_dropstack(pf, &(pf->stack))

void _pf_push(pf *pf, _ ob) {
    pf_void(pf);
    TOP = ob;
}
_ _pf_make_symbol(pf *pf, const char *str){
    return const_to_object(symbol_from_string(TYPES->symbol_type, str));
}



/* INTERPRETER */

// CODE  = RETURN | (SUB : CODE)    ;; ':' is a GC CONS
// SUB   = PRIM | CODE
//
// RS    = MT | (CODE . RS)         ;; '.' is a linear CONS

typedef void (*pf_prim)(pf*);

#define RETURN CONSTANT(0x200)

void pf_run(pf *pf) {
    code *c, *csub;
    prim *p;
    quote *q;
    box *b;
    lin *l;

    /* Toplevel exceptions. */
    while (setjmp(pf->m.top));

  loop:
    /* Interpeter loop. */
    for(;;) {
        if ((c = object_to_code(pf->ip))) {
            /* Threaded code subroutine */
            if ((csub = object_to_code(c->sub))) {
                pf->rs = _pf_cons(pf, csub->next, pf->rs);
                pf->ip = csub->sub;
            }
            /* Primitive */
            else if ((p = object_to_prim(c->sub, &pf->m))) {
                pf_prim fn = (pf_prim)p->fn;
                pf->m.r.prim = p;
                switch(setjmp(pf->m.r.step)) {
                case 0:
                    fn(pf);
                    pf->ip = c->next;
                    break;
                default:
                    _pf_push(pf, SYMBOL("unknown-exception"));
                case PF_EX_ABORT:
                    pf->ip = pf->ip_abort;
                    break;
                }
            }
            /* Quoted object */
            else if ((q = object_to_quote(c->sub))) {
                _ ob;
                if ((l = object_to_lin(q->object))) {
                    /* Linear objects need to be copied. */
                    ob = _pf_link(pf, l->object);
                }
                else {
                    /* All other objects behave as constants to the
                       linear memory manager. */
                    if (unlikely(object_to_pair(q->object))) pf_trap(pf);
                    ob = q->object;
                }
                _pf_push(pf, ob);
                pf->ip = c->next;
            }
            else {
                pf_trap(pf);
            }
        }
        else if ((RETURN == pf->ip)) {
            if (unlikely(NIL == pf->rs)) goto halt;
            pf->ip = CAR(pf->rs);
            DROP(rs);
        }
        else {
            /* ERROR */
            pf_trap(pf);
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
    else if ((x = object_to_code(ob))) {
        return _ex_printf(EX, "#code<%p>", x);
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

void pf_trap(pf *pf) { 
    kill(getpid(), SIGTRAP);
}
void pf_void(pf *pf) {
    pf->ds = _pf_cons(pf, VOID, pf->ds);
}
void pf_drop(pf *pf) {
    DROP(ds);
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


void pf_dup_write(pf *pf) { px_write(pf, TOP); }
void pf_dup_post(pf *pf)  { px_post(pf, TOP); }

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
    CAR(pf->rs) = MOVE(pf->ds, NIL);
    FROM_TO(rs, ds);
}
void pf_print_error(pf *pf) {
    if (NIL == pf->ds) _pf_push(pf, VOID);
    _ex_printf(EX, "ERROR: ");
    px_post(pf, TOP);
    pf_drop(pf);
}

/* Convert a list to code and jump to it. */
_ ex_compile(ex *ex, _ obj) {
    return obj;
}

//void pf_eval(pf *pf) {
    // _ code = _pf_eval(pf, TOP);
//}



/* GC + SETUP */

static void _pf_restart(pf* pf) {
    longjmp(pf->m.top, PF_EX_RESTART);
}
static void _pf_overflow(pf *pf, long extra) {
    printf(";; gc-overflow\n");
    /* At this point, the heap is compacted, but the requested
       allocation doesn't fit.  We need to grow.  Take at least the
       requested size + grow by a fraction of the total heap. */
    long request = extra + (GC->slot_total/4);
    _ex_printf(EX, ";; gc-overflow %ld:%ld\n", extra, request);
    gc_grow(GC, request);
    _pf_restart(pf);
}
static void _pf_mark_roots(pf *pf, gc_finalize fin) {
    printf(";; gc\n");
    gc_mark(GC, pf->ds);
    gc_mark(GC, pf->rs);
    gc_mark(GC, pf->free);
    gc_mark(GC, pf->dict);
    _pf_restart(pf);
}

static _ _pf_prim(pf* pf, pf_prim fn) {
    prim *p = malloc(sizeof(*p));
    p->type = TYPES->prim_type;
    p->fn = fn;
    p->nargs = 0;
    p->var = VOID;
    return const_to_object(p);
}

#define CODE(a,b) _pf_code(pf, a, b)
#define QUOTE(a)  _pf_quote(pf, a)
#define PRIM(fn)  _pf_prim(pf, fn)

pf* _pf_new(void) {
    pf *pf = malloc(sizeof(*pf));

    // Garbage collector.
    GC = gc_new(100000, pf, 
                (gc_mark_roots)_pf_mark_roots,
                (gc_overflow)_pf_overflow);

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
    pf->ip_abort = 
        CODE(PRIM(pf_print_error), RETURN);
    pf->ip = 
        CODE(PRIM(pf_output),
             CODE(PRIM(pf_output),
                  CODE(QUOTE(NUMBER(123)),
                       CODE(PRIM(pf_state), RETURN))));

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
