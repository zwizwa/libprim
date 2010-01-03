// Hack it on top of SC

#define SC_NEW_VM
#include <sc/scheme.c>
#include <mz/vm.h_prims>

/* The VM uses vectors tagged with a native code pointer to represent
   primitive code and continuations.  

   VM opcodes modify the machine state and possibly install new
   continuation frames in case evaluation is required.

   Continuation opcodes take a value argument and complete the
   computation. */

typedef void (*k_code)(sc *sc, _ value, void *frame);
typedef struct {
    vector v;
    k_code tag; // VM code point to jump to
    _ parent;   // parent continuation frame
    _ env;      // lexical context associated w. vale hole
    // _ marks;
} kf_base;

typedef void (*vm_code)(sc *sc, void *opcode);
typedef struct {
    vector v;
    vm_code fn;
} vm_op;






/* The following contains code for the VM primitives and continuation
  primitives.  These are defined close to each other to keep the code
  cache footprint small. */

/* VM ops creating continuation frames. */
#define CHECK_ALIGNED(x) { if(((void*)x) != GC_POINTER((object)x)) TRAP(); }

static kf_base *_sc_extend_kf_base(sc *sc, int slots, void *tag) {
    kf_base *kf = (kf_base*)gc_alloc(EX->gc, slots);
    kf->parent = sc->k;
    kf->env    = sc->e;
    sc->k = vector_to_object((vector*)kf);
    kf->tag = (k_code)tag;
    CHECK_ALIGNED(tag);
    return kf;
}

#define EXTEND_K(type, name) \
    type *name = (type*)_sc_extend_kf_base\
        (sc, ((sizeof(type))/sizeof(_))-1, _##type);

typedef kf_base kf_halt;
static void _kf_halt(sc *sc, _ v) {
    ERROR("vm-halt", v);
}


/* VM OPCODES. */

/* Each instruction modifies the current state in-place, possibly
   extending the continuation frame. */

/* Pass value to current continuation and pop frame. */
static void _value(sc *sc, _ value) {
    kf_base *f = (kf_base*)GC_POINTER(sc->k); // unsafe
    sc->k = f->parent;                        // pop frame
    sc->e = f->env;                           // restore lex env
    k_code fn = f->tag;
    fn(sc, value, f);
}

/* IF */
typedef struct {
    vm_op op;
    _ value;
    _ yes;
    _ no;
} vm_if;
static void _vm_if(sc *sc, vm_if *op) {
    sc->c = (FALSE == op->value) ? op->no : op->yes;
}

/* LIT */
typedef struct {
    vm_op op;
    _ value;
} vm_lit;
static void _vm_lit(sc *sc, vm_lit *op) {
    _value(sc, op->value);
}

/* REF */
typedef struct {
    vm_op op;
    _ index;
} vm_ref;
static _ _sc_ref(sc *sc, _ index) {
    int n = object_to_integer(index);
    _ env = sc->e;
    while(n--) env = _CDR(env);
    return _CAR(env);
}
static void _vm_ref(sc *sc, vm_ref *op) {
    _value(sc, _sc_ref(sc, op->index));
}

/* BOX / UNBOX */
typedef struct {
    vector v;
    _ value;
} box;
typedef struct {
    vm_op op;
    _ box;
} op_unbox;
static void _vm_unbox(sc *sc, op_unbox *op) {
    box *b = (box *)GC_POINTER(op->box);
    _value(sc, b->value);
}
typedef struct {
    vm_op op;
    _ box;
    _ value;
} op_setbox;
static void _vm_setbox(sc *sc, op_setbox *op) {
    box *b = (box *)GC_POINTER(op->box);
    b->value = op->value;
    _value(sc, VOID);
}


/* PRIM */
typedef struct {
    vm_op op;
    _ fn;
    _ ids;  // De Bruijn indices
} vm_prim;
#define ARG(n) _sc_ref(sc, v->slot[n])
void _vm_prim(sc *sc, vm_prim *op) {
    vector *v = (vector *)(GC_POINTER(op->ids));
    void *p = GC_POINTER(op->fn);
    _ rv;
    switch(vector_size(v)) {
    case 0: rv = ((ex_0)p)(EX); break;
    case 1: rv = ((ex_1)p)(EX, ARG(0)); break;
    case 2: rv = ((ex_2)p)(EX, ARG(0), ARG(1)); break;
    case 3: rv = ((ex_3)p)(EX, ARG(0), ARG(1), ARG(2)); break;
    case 4: rv = ((ex_4)p)(EX, ARG(0), ARG(1), ARG(2), ARG(3)); break;
    case 5: rv = ((ex_5)p)(EX, ARG(0), ARG(1), ARG(2), ARG(3), ARG(4)); break;
    default: ERROR("nargs", op->ids);
    }
    _value(sc, rv);
}

/* APPLY */
typedef struct {
    vector v;
    _ env;
    _ body;
} closure;
typedef struct {
    vm_op op;
    _ closure;
    _ ids;  // De Bruijn indices
} vm_app;
void _vm_app(sc *sc, vm_app *op) {
    closure *cl = (closure*)GC_POINTER(_sc_ref(sc, op->closure));
    vector *v = (vector *)GC_POINTER(op->ids);
    _ env = cl->env;
    int i, n = vector_size(v);
    for(i = n-1; i >= 0; i--) {
        _ val = _sc_ref(sc, v->slot[i]);
        env = CONS(val, env);
    }
    sc->e = env;
    sc->c = cl->body;
}
typedef struct {
    vm_op op;
    _ body;
} vm_lambda;
void _vm_lambda(sc *sc, vm_lambda *op) {
    _ cl = STRUCT(TAG_VECTOR, 2, sc->e, op->body);
    _value(sc, cl);
}

/* SEQ */
typedef struct {
    kf_base f;
    _ next;
} kf_seq;
static void _kf_seq(sc *sc, _ v, kf_seq *f) {
    sc->c = f->next;
}
typedef struct {
    vm_op op;
    _ now;
    _ later;
} vm_seq;
void _vm_seq(sc *sc, vm_seq *op) {
    EXTEND_K(kf_seq, f);
    sc->c   = op->now;
    f->next = op->later;
}

/* LET1 */
typedef struct {
    kf_base f;
    _ body;
} kf_let1;
static void _kf_let1(sc *sc, _ v, kf_let1 *f) {
    sc->e = CONS(v, sc->e); // extend environemnt
    sc->c = f->body;
}
typedef struct {
    vm_op op;
    _ expr;
    _ body;
} vm_letvar;
void _vm_let1(sc *sc, vm_letvar *op) {
    EXTEND_K(kf_let1, f);
    sc->c   = op->expr; 
    f->body = op->body;
}


_ sc_vm_continue(sc *sc) {
    for(;;) {
        vm_op *op = (vm_op*)GC_POINTER(sc->c);
        op->fn(sc, op);
    }
}




/* Opcode construction. */
#define OP(name, nargs, ...)                                            \
    return gc_make_tagged(EX->gc, TAG_VECTOR, (1+nargs),                \
                              const_to_object(_vm_##name), __VA_ARGS__)


_ sc_op_if(sc *sc, _ cval, _ yes, _ no)  {OP(if,     3, cval, yes, no);}
_ sc_op_lit(sc *sc, _ val)               {OP(lit,    1, val);}
_ sc_op_ref(sc *sc, _ id)                {OP(ref,    1, id);}
_ sc_op_prim(sc *sc, _ fn, _ ids)        {OP(prim,   2, fn, ids);}
_ sc_op_seq(sc *sc, _ now, _ later)      {OP(seq,    2, now, later);}
_ sc_op_let1(sc *sc, _ expr, _ body)     {OP(let1,   2, expr, body);}
_ sc_op_unbox(sc *sc, _ box)             {OP(unbox,  1, box);}
_ sc_op_setbox(sc *sc, _ box, _ val)     {OP(setbox, 2, box, val);}
_ sc_op_app(sc *sc, _ closure, _ ids)    {OP(app,    2, closure, ids);}
_ sc_op_lambda(sc *sc, _ body)           {OP(lambda, 1, body);}

_ sc_prim_fn(sc *sc, _ p) { 
    void *fn = CAST(prim, p)->fn;
    CHECK_ALIGNED(fn);
    return const_to_object(fn);
}

_ sc_vm_init(sc *sc, _ c) {
    sc->e = NIL;
    sc->k = NIL;
    sc->c = c;
    EXTEND_K(kf_halt, f);
    return VOID;
}

static prim_def vm_prims[] = vm_table_init;
int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, vm_prims);
    _sc_continue(sc);
    return 0;
}
