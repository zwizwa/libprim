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






/* The following contains code for the VM opcode and continuation
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


/* LIT */
typedef struct {
    vm_op op;
    _ value;
} vm_lit;
static void _vm_lit(sc *sc, vm_lit *op) {
    _value(sc, op->value);
}

/* REF */
static _ _sc_ref_pair(sc *sc, _ index) {
    int n = object_to_integer(index);
    _ env = sc->e;
    while(n--) env = _CDR(env);
    return env;
}
static _ _sc_ref(sc *sc, _ index) {
    return _CAR(_sc_ref_pair(sc, index));
}
typedef struct {
    vm_op op;
    _ index;
} vm_ref;
static void _vm_ref(sc *sc, vm_ref *op) {
    _value(sc, _sc_ref(sc, op->index));
}

/* ASSIGN */
typedef struct {
    vm_op op;
    _ id_variable;
    _ id_value;
} vm_assign;
static _ _vm_assign(sc *sc, vm_assign *op) {
    _CAR(_sc_ref_pair(sc, op->id_variable)) = _sc_ref(sc, op->id_value);
    _value(sc, VOID);
}

/* IF */
typedef struct {
    vm_op op;
    _ id_value;
    _ yes;
    _ no;
} vm_if;
static void _vm_if(sc *sc, vm_if *op) {
    sc->c = (FALSE == _sc_ref(sc, op->id_value)) 
        ? op->no : op->yes;
}


/* APPLY */

/* Application operates on operator values that have unknown type at
   compile time, which means they need to be implemented as tagged
   structs instead of plain vectors. */

// FIXME: these overlap with the other interpreter's primitive structs.
#define TAG_CLOSURE VECTOR_TAG(7)
typedef struct {
    vector v;
    _ env;
    _ body;
    _ signature;
} closure;
DEF_STRUCT(closure, TAG_CLOSURE)

_ _extend(sc *sc, _ env, _ cl_body, int named_args, int list_args, _ op_ids) {
    vector *v = (vector *)GC_POINTER(op_ids);
    int i, n = vector_size(v);

    /* Gather list args. */
    if (list_args) {
        _ la = NIL;
        int nb_list_args = n - named_args;
        if (n < 0) ERROR("nargs", op_ids);
        for (i = n-1; i >= named_args; i--) {
            _ val = _sc_ref(sc, v->slot[i]);
            la = CONS(val, la);
        }
        env = CONS(la, env);
        n = named_args;
    }

    /* Check nb of arguments. */
    if (n != named_args) ERROR("nargs", op_ids);

    /* Gather named arguments. */
    for(i = n-1; i >= 0; i--) {
        _ val = _sc_ref(sc, v->slot[i]);
        env = CONS(val, env);
    }
    
    /* Start executing body in extended context. */
    return env;
}
typedef struct {
    vm_op op;
    _ id_rator;
    _ id_args;  // De Bruijn indices
} vm_app;

/* Application dispatches on operator type. */
void _vm_app(sc *sc, vm_app *op) {
    closure *cl;
    prim *pr;
    _ rator = _sc_ref(sc, op->id_rator);
    if ((cl = object_to_closure(rator))) {
        /* LSB of signature indicates if there's an extra formal
           referring to the remaining arguments. */
        int named_args = object_to_integer(cl->signature);
        int list_args = named_args & 1;
        named_args >>= 1;
        /* Extend environment and jump to body. */
        sc->e = _extend(sc, cl->env, cl->body, named_args, list_args, op->id_args);
        sc->c = cl->body;
    }
    else if ((pr = object_to_prim(rator))) {
        /* Extend NIL environment, perform primitive and pass value to
           current continuation. */
        _ args = _extend(sc, NIL, NIL, pr->nargs, 0, op->id_args);
        _ rv = _sc_call(sc, pr->fn, pr->nargs, args);
        _value(sc, rv);
    }
    else {
        TYPE_ERROR(rator);
    }
}

/* LAMBDA */
typedef struct {
    vm_op op;
    _ body;
    _ signature;
} vm_lambda;
void _vm_lambda(sc *sc, vm_lambda *op) {
    _ cl = STRUCT(TAG_CLOSURE, 3, sc->e, op->body, op->signature);
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

/* LET */
typedef struct {
    kf_base f;
    _ env;
    _ exprs;
    _ body;
} kf_let;
static void _kf_let(sc *sc, _ v, kf_let *f) {
    /* Extend environment with value. */
    _ env = CONS(v, f->env);
    if (NIL == f->exprs) {
        /* Done: enter new environment. */
        sc->e = env;
        sc->c = f->body;
    }
    else {
        /* Evaluate next expression. */
        EXTEND_K(kf_let, fnext);
        sc->c        = _CAR(f->exprs);
        fnext->exprs = _CDR(f->exprs);
        fnext->body  = f->body;
        fnext->env   = env;
    }
}
typedef struct {
    vm_op op;
    _ exprs;
    _ body;
} vm_let;
void _vm_let(sc *sc, vm_let *op) {
    EXTEND_K(kf_let, f);
    sc->c    = _CAR(op->exprs);
    f->exprs = _CDR(op->exprs);
    f->body  = op->body;
    /* Build an extended environement with computed values, but stay
       in the current one during evaluation. */
    f->env   = sc->e;
}

/* Trampoline. */
_ sc_vm_continue(sc *sc) {
    for(;;) {
        vm_op *op = (vm_op*)GC_POINTER(sc->c);
        op->fn(sc, op);
    }
}


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



/* Opcode construction. */
#define MAKE_OP(name, nargs, ...)                                       \
    gc_make_tagged(EX->gc, TAG_VECTOR, (1+nargs),                       \
                              const_to_object(_vm_##name), __VA_ARGS__)
#define OP(name, ...) return MAKE_OP(name, __VA_ARGS__)

_ sc_op_if(sc *sc, _ cval, _ yes, _ no)  {OP(if,     3, cval, yes, no);}
_ sc_op_lit(sc *sc, _ val)               {OP(lit,    1, val);}
_ sc_op_ref(sc *sc, _ id)                {OP(ref,    1, id);}
_ sc_op_seq(sc *sc, _ now, _ later)      {OP(seq,    2, now, later);}
_ sc_op_let(sc *sc, _ exprs, _ body)     {OP(let,    2, exprs, body);}
_ sc_op_app(sc *sc, _ closure, _ ids)    {OP(app,    2, closure, ids);}
_ sc_op_lambda(sc *sc, _ body, _ sig)    {OP(lambda, 2, body, sig);}
_ sc_op_assign(sc *sc, _ id, _ val)      {OP(assign, 2, id, val);}


/* Compile s-expression representing Scheme code in ANF form to
   internal representation.  This allows simpler bootstrapping, as no
   external byte-code representation is necessary; we can simply use
   s-expressions and the leaf/parser.c code. */
#define V1 CADR(code)
#define V2 CADDR(code)
#define V3 CADR(CDDR(code))
#define ANF(...) sc_vm_compile_anf(sc, __VA_ARGS__)
#define ANFS(ES) _ex_map1_prim(EX, (ex_1)sc_vm_compile_anf, ES)
#define E1 ANF(V1)
#define E2 ANF(V2)
#define E3 ANF(V3)
#define ES1 ANFS(V1)  // recurse down list of expressions
#define CASE_OP(str, code, ...) if(tag == SYMBOL(str)) return MAKE_OP(code, __VA_ARGS__)

_ sc_vm_compile_anf(sc *sc, _ code) {
    _ tag = CAR(code);

    CASE_OP("%let",    let,    2, ES1, E2);
    CASE_OP("%seq",    seq,    2,  E1, E2);

    CASE_OP("%if",     if,     3,  V1, E2, E3);
    CASE_OP("%ref",    ref,    1,  V1);
    CASE_OP("%lit",    lit,    1,  V1);
    CASE_OP("%app",    app,    2,  V1, V2);
    CASE_OP("%lambda", lambda, 2,  E1, V2);
    CASE_OP("%assign", assign, 2,  V1, V2);

    return ERROR("invalid-opcode", tag);
}



static prim_def vm_prims[] = vm_table_init;
int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, vm_prims);
    _sc_continue(sc);
    return 0;
}
