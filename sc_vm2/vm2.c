#define SC_NEW_VM

#include <sc/sc.c>

#include <sc_vm1/vm1.c>     // piggy-back on VM1
#include <sc_vm2/vm2.h_prims>

/* The VM interprets code trees and continuation stacks, represented
   by nested vectors tagged with native code pointers.

   CODE: VM opcodes modify the machine state.  %let and %seq push new
         continuation frames to the current execution context to
         keep track of nested expression evaluation.

   CONT: Continuation opcodes take a value argument and complete the
         computation.
*/

typedef void (*k_code)(sc *sc, _ value, void *frame);
typedef struct {
    vector v;
    k_code tag; // VM code point to jump to
    _ parent;   // parent continuation frame
    _ env;      // lexical context associated w. value hole
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

static kf_base *_sc_extend_kf_base(sc *sc, int slots, void *tag) {
    kf_base *kf = (kf_base*)gc_alloc(EX->gc, slots);
    kf->parent = sc->k;
    kf->env    = sc->e;
    sc->k = vector_to_object((vector*)kf);
    kf->tag = (k_code)tag;
    GC_CHECK_ALIGNED(tag);
    return kf;
}

#define EXTEND_K(type, name) \
    type *name = (type*)_sc_extend_kf_base\
        (sc, ((sizeof(type))/sizeof(_))-1, _##type);

typedef kf_base kf_halt;
static void _kf_halt(sc *sc, _ v) {
    HALT_VM(v);
}


/* VM OPCODES. */

/* Each instruction modifies the current state in-place, possibly
   extending the continuation frame. */

/* Pass value to current continuation and pop frame. */
static void _return_value(sc *sc, _ value) {
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
    _return_value(sc, op->value);
}

/* REF */
#define IS_LEXICAL_VARIABLE(x) (GC_INTEGER == GC_TAG(x))
static _* _sc_box(sc *sc, _ ref) {
    if (IS_LEXICAL_VARIABLE(ref)) {
        int n = object_to_integer(ref);
        _ env = sc->e;
        while(n--) env = _CDR(env);
        return &_CAR(env);
    }
    else {
        _ slot = FIND_SLOT(TOPLEVEL(), ref); 
        if (slot != NIL) return &_CDR(slot);
        else { ERROR_UNDEFINED(ref); return NULL; }
    }
}
static _ _sc_varref(sc *sc, _ ref) {
    return *_sc_box(sc, ref);
}
typedef struct {
    vm_op op;
    _ index;
} vm_ref;

/* Unpack a value wrapped in byte code.  The GC integer type represent
   variable references.  Other values are wrapped. */
_ _unpack_value(sc *sc, _ it) {
    return _sc_varref(sc, it);
}

static void _vm_ref(sc *sc, vm_ref *op) {
    _return_value(sc, _unpack_value(sc, op->index));
}


/* ASSIGN */
typedef struct {
    vm_op op;
    _ id_variable;
    _ id_value;
} vm_assign;
static void _vm_assign(sc *sc, vm_assign *op) {
    *_sc_box(sc, op->id_variable) = _unpack_value(sc, op->id_value);
    _return_value(sc, VOID);
}

/* IF */
typedef struct {
    vm_op op;
    _ id_value;
    _ yes;
    _ no;
} vm_if;
static void _vm_if(sc *sc, vm_if *op) {
    sc->c = (FALSE == _unpack_value(sc, op->id_value)) 
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


/* Extend enironment. */
_ _extend(sc *sc, _ env, _ cl_body, int named_args, int list_args, _ op_ids) {
    vector *v = (vector *)GC_POINTER(op_ids);
    int i, n = vector_size(v);

    /* Gather list args. */
    if (list_args) {
        _ la = NIL;
        int nb_list_args = n - named_args;
        if (n < 0) ERROR("nargs", op_ids);
        for (i = n-1; i >= named_args; i--) {
            _ val = _unpack_value(sc, v->slot[i]);
            la = CONS(val, la);
        }
        env = CONS(la, env);
        n = named_args;
    }

    /* Check nb of arguments. */
    if (n != named_args) ERROR("nargs", op_ids);

    /* Gather named arguments. */
    for(i = n-1; i >= 0; i--) {
        _ val = _unpack_value(sc, v->slot[i]);
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
    _ rator = _unpack_value(sc, op->id_rator);
    if ((cl = object_to_closure(rator))) {
        /* LSB of signature indicates if there's an extra formal
           referring to the remaining arguments. */
        int named_args = object_to_integer(cl->signature);
        int list_args = named_args & 1;
        named_args >>= 1;
        /* Extend environment and jump to body. */
        sc->e = _extend(sc, cl->env, cl->body, named_args,
                        list_args, op->id_args);
        sc->c = cl->body;
    }
    else if ((pr = object_to_prim(rator))) {
        /* Extend NIL environment to obtain primitive's argument list,
           perform primitive and pass value to current continuation. */
        _ args = _extend(sc, NIL, NIL, pr->nargs, 0, op->id_args);
        _ rv = _sc_call(sc, pr->fn, pr->nargs, args);
        _return_value(sc, rv);
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
    _return_value(sc, cl);
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
typedef struct {
    vm_op op;
    _ exprs;
    _ body;
} vm_let;
static void _kf_let(sc *sc, _ v, kf_let *f);
/* Evaluate list of expressions by creating new continuation frames.
   Only create frames for closure applications.  If list is empty,
   evaluate body in new environment. */
static void _let(sc *sc, _ exprs, _ body, _ env) {
    if (NIL == exprs) {
        /* Done: enter new environment. */
        sc->e = env;
        sc->c = body;
    }
    else {
        _ next  = _CAR(exprs);
        _ later = _CDR(exprs);

        /* Evaluate next expression. */
        EXTEND_K(kf_let, f);
        sc->c    = next;
        f->exprs = later;
        f->body  = body;
        f->env   = env;
    }
}
/* Extend environment with value and continue. */
static void _kf_let(sc *sc, _ v, kf_let *f) {
    _ env = CONS(v, f->env);
    _let(sc, f->exprs, f->body, env);
}
static void _vm_let(sc *sc, vm_let *op) {
    _let(sc, op->exprs, op->body, sc->e);
}

/* Trampoline. */
_ sc_vm_eval_bytecode(sc *sc, _ expr) {
    _ c = sc->c;  sc->c = expr;
    _ e = sc->e;  sc->e = NIL;
    _ k = sc->k;  sc->k = NIL; EXTEND_K(kf_halt, f);
    int err;
    typeof(sc->m.l.buf) save;


    // _ex_printf(EX, "Saved %d bytes\n", sizeof(save));

    memcpy(&save, &sc->m.l.buf, sizeof(save));


    switch((err = leaf_catch(sc))) {
    case EXCEPT_TRY:
        for(;;) {
            vm_op *op = (vm_op*)GC_POINTER(sc->c);
            op->fn(sc, op);
        }
    default:
        sc->c = c;
        sc->e = e;
        sc->k = k;
        memcpy(&sc->m.l.buf, &save, sizeof(save));
        if (err == EXCEPT_HALT) return CAST(error, sc->error)->arg;
        _ex_printf(EX, "Exception %d during vm-eval\n", err);
        longjmp(sc->m.l.buf, err);
    }
}

/* Compile s-expression representation of byte code to internal
   representation.  This allows simpler bootstrapping, as no external
   byte-code representation is necessary; we can simply use
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
#define MAKE_OP(name, nargs, ...)                                       \
    gc_make_tagged(EX->gc, TAG_VECTOR, (1+nargs),                       \
                              const_to_object(_vm_##name), __VA_ARGS__)
#define CASE_OP(str, code, ...) \
    if(tag == SYMBOL(str)) return MAKE_OP(code, __VA_ARGS__)

_ sc_vm_compile_anf(sc *sc, _ code) {
    _ tag = CAR(code);
    CASE_OP("op-let",    let,    2, ES1, E2);
    CASE_OP("op-seq",    seq,    2,  E1, E2);
    CASE_OP("op-if",     if,     3,  V1, E2, E3);
    CASE_OP("op-ref",    ref,    1,  V1);
    CASE_OP("op-lit",    lit,    1,  V1);
    CASE_OP("op-app",    app,    2,  V1, V2);
    CASE_OP("op-lambda", lambda, 2,  E1, V2);
    CASE_OP("op-assign", assign, 2,  V1, V2);
    return ERROR("invalid-opcode", tag);
}

static prim_def vm_prims[] = vm2_table_init;
int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, vm_prims);
    _sc_continue(sc);
    return 0;
}
