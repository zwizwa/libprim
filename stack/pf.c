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

#include "mem.h"
#include "gc.h"

#define TOP ARG0
#define ARG0 CAR(pf->ds) 
#define ARG1 CADR(pf->ds)


typedef struct {
    mem m;

    /* Linear memory. */
    _ ds;    // parameter stack
    _ rs;    // retain stack
    _ free;  // free list
    _ output;

    /* Graph memory. */
    _ ip;
    _ dict;

} pf;

#include "pf2.h_"


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
    return gc_cons(pf->m.gc, VOID, NIL);
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
void pf_error_underflow(pf *pf);
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

void *object_rc_struct(object ob, mem *m, void *type) {
    rc *x = object_to_rc(ob, m);
    if (!x) return NULL;
    void *x_type = *((void**)x->object);
    if (x_type != type) return NULL;
    return x->object;
}
_ _pf_make_rc(pf *pf, void *free, void *object) {
    rc *rc = malloc(sizeof(*rc));
    rc->type = pf->m.rc_type;
    rc->free = free;
    rc->object = object;
    rc->rc = 1;
    return const_to_object(rc);
}

/* RC types are const types wrapped in a RC struct. */
#define DEF_RC_TYPE(name)                                              \
    static inline name *object_to_##name(object ob, mem *m) {          \
        return (name*)object_rc_struct(ob,m,m->name##_type); }
DEF_RC_TYPE(port)
static _ _pf_make_port(pf *pf, FILE *f, const char *name) {
    return _pf_make_rc(pf, 
                       pf->m.port_type->free,
                       port_new(pf->m.port_type, stdout, name));
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
        x->free(x->object);
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

   Both linear _value_ and _variable_ structs are implemented as
   finalizer wrappers based on aref. */

typedef aref box;
typedef aref lin;
#define TAG_LIN   VECTOR_TAG(12)
#define TAG_BOX   VECTOR_TAG(13)
DEF_STRUCT(lin,   TAG_LIN)
DEF_STRUCT(box,   TAG_BOX)

static void _gc_unlink(_ ob, pf *pf) { _pf_unlink(pf, ob); }
static void *unlink_fin = _gc_unlink;

/* There are 2 kinds of data structures hold linear values: a mutable
   box, and an immutable value.  BOX values are treated as graph
   values (constants), while LIN values are always unpacked into
   linear memory. */
static inline _ _pf_box(pf *pf, _ ob) {
    return gc_make_tagged(pf->m.gc, TAG_BOX, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
static inline _ _pf_lin(pf *pf, _ ob) {
    return gc_make_tagged(pf->m.gc, TAG_LIN, 2, 
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
        return gc_cons(pf->m.gc,
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



/* INTERPRETER */

typedef struct {
    vector v;
    _ object;
} quote;
typedef struct {
    vector v;
    _ sub;
    _ next;
} code;

#define TAG_QUOTE VECTOR_TAG(14)
#define TAG_CODE  VECTOR_TAG(15)
DEF_STRUCT(quote, TAG_QUOTE)
DEF_STRUCT(code,  TAG_CODE)

static inline _ _pf_quote(pf *pf, _ data) { 
    return gc_make_tagged(pf->m.gc, TAG_QUOTE, 1, data);
}
static inline _ _pf_code(pf *pf, _ sub, _ next) { 
    return gc_make_tagged(pf->m.gc, TAG_CODE, 2, sub, next);
}

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
    for(;;) {
        if ((c = object_to_code(pf->ip))) {
            /* Threaded code subroutine */
            if ((csub = object_to_code(c->sub))) {
                pf->rs = _pf_cons(pf, csub->next, pf->rs);
                pf->ip = csub->sub;
            }
            /* Primitive */
            else if ((p = object_to_prim(c->sub, &pf->m))) {
                pf_prim fn = (pf_prim)(p->fn);
                fn(pf);
                pf->ip = c->next;
            }
            /* Quoted object */
            else if ((q = object_to_quote(c->sub))) {
                /* FIXME: distinguish between arefs and constants.
                   Can't ref lists here! */
                _pf_push(pf, q->object);
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
    return;
}

/* PRIMITIVES */

void pf_trap(pf *pf) { 
    kill(getpid(), SIGTRAP);
}

void pf_error_underflow(pf *pf) {
    fprintf(stderr, "stack underflow\n");
    pf_trap(pf);
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
    pf->dict = gc_cons(pf->m.gc, ob, pf->dict);
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


typedef struct { 
    pf *pf;
    port *p;
    mem *m;
} _write_ctx_;
static _ _write_delegate(_write_ctx_ *ctx, _ ob) {
    port *p = ctx->p;
    mem *m = ctx->m;
    object_write_delegate fn = (object_write_delegate)_write_delegate;
    void *x;
    if (FALSE == object_write(ob, p, m, fn, ctx)) {
        if ((x = object_to_port(ob, m))) {
            object_write(const_to_object(x), p, m, fn, ctx);
        }
        else if ((x = object_to_code(ob))) {
            port_printf(p, "#code<%p>", x);
        }
        else {
            port_printf(p, "#object<%p>",(void*)ob);
        }
    }
    return VOID;
}

void _pf_write(pf *pf, _ ob) {
    _write_ctx_ ctx = {pf, object_to_port(pf->output, &pf->m), &pf->m};
    _write_delegate(&ctx, ob);
    port_printf(ctx.p, " ");
}
void _pf_post(pf *pf, _ ob) {
    _pf_write(pf, ob);
    port *p = object_to_port(pf->output, &pf->m);
    port_printf(p, "\n");
}

void pf_dup_write(pf *pf) { _pf_write(pf, TOP); }
void pf_dup_post(pf *pf) { _pf_post(pf, TOP); }
void pf_state(pf *pf) {
    port *p = object_to_port(pf->output, &pf->m);
    port_printf(p, "P: "); _pf_post(pf, pf->ds);
    port_printf(p, "R: "); _pf_post(pf, pf->rs);
    port_printf(p, "F: "); _pf_post(pf, pf->free);
    port_printf(p, "D: "); _pf_post(pf, pf->dict);
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

static void _pf_overflow(pf *pf, long extra) {
    printf(";; gc-overflow\n");
    pf_trap(pf);
}
static void _pf_mark_roots(pf *pf, gc_finalize fin) {
    printf(";; gc\n");
    gc_mark(pf->m.gc, pf->ds);
    gc_mark(pf->m.gc, pf->rs);
    gc_mark(pf->m.gc, pf->free);
    gc_mark(pf->m.gc, pf->dict);
    pf_trap(pf);
}

static _ _pf_prim(pf* pf, pf_prim fn) {
    prim *p = malloc(sizeof(*p));
    p->type = pf->m.prim_type;
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
    pf->m.gc = gc_new(100000, pf, 
                     (gc_mark_roots)_pf_mark_roots,
                     (gc_overflow)_pf_overflow);

    // Leaf types.
    pf->m.ck_type = ck_class_new();
    pf->m.symbol_type = symbol_class_new(1000);
    pf->m.port_type = port_class_new();
    pf->m.prim_type = (void*)0xF001; 
    pf->m.rc_type = (void*)0xF002; 

    // Machine state.
    pf->rs = NIL;
    pf->ds = NIL;
    pf->free = NIL;
    pf->dict = NIL;

    pf->ip = CODE(PRIM(pf_output), 
                  CODE(QUOTE(integer_to_object(123)),
                       CODE(PRIM(pf_state), RETURN)));
    

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
