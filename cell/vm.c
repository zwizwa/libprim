/* VM on top of CELL.  

   The main idea behind the CELL graph memory is to run on ARM cores
   with little memory (up to 64k cells using 256 kB RAM).

   For the interpreter it would be convenient to:
     - have compact byte code
     - run code in graph memory
     - run code in flat (constant) Flash memory (*)

   The core language is a variant of ANF[1] where all values are
   acessed indirectly through variable references.


   The VM is in the first place simple: all structures are represented
   as pair based binary trees.  This means it generates a lot of
   garbage in the form of environments and continuations.

   However the cell.c GC has mark phase complexity proportional to the
   live set, and a separate tag store.  The latter uses optimized
   traversal during pre-mark cell tagging and lazy allocation phases.

   (*) Running code in Flash or other non-managed memory requires
   re-directing implemenations of CAR/CDR and NCAR/NCDR.

   [1] http://en.wikipedia.org/wiki/Administrative_normal_form

*/

#include "cell.h"

struct _vm {
    cell *c;
    cell *e;
    cell *k;
    cell *v;
    cell *a;
    cell *t;
    void *END;
} __attribute((__packed__));
#define VM_INIT {NIL,NIL,NIL,NIL,NIL,0};

typedef struct _vm vm;

typedef void (*vm_prim)(vm *vm);


/* Get environment slot and ref. */
cell* e_slot(cell *e, int i) {
    while(i--) e = CDR(e);
    return e;
}
cell* e_ref(cell *e, int i) {
    return CAR(e_slot(e, i));
}
void e_set(cell *e, int i, cell *v) {
    SET_CAR(e_ref(e, i), v);
}


/* VM main loop.

   The basic idea is the same as in most other libprim VMs.

   The machine has 3 registers:

   C: the current code to be interpreted
   E: the current variable binding environment (CDR linked list).
   K: the current continuation, (CDR linked stack of frames).
   

   There are 2 classes of actions:

   - OPCODES: These represent the primitive forms of the language:
     let, begin, set!, app, if, quote.  The first two produce new
     continuation frames.

   - KCODES:  These represent the primitive forms that are executed
     whenever evaluation finishes, and the top K frame is invoked.
*/

void vm_continue(vm *vm) {
    int i;              // current opcode / variable index

/* Registers are in the vm struct so the GC can see them. */
#define c     (vm->c)   // expression
#define e     (vm->e)   // lexical environment
#define k     (vm->k)   // execution context
#define v     (vm->v)   // value register
#define arg   (vm->a)   // argument to op_ or k_
#define e_ext (vm->t)   // temp env

/* Opcodes encoded as NUMBER(). */
#define K_HALT  NUMBER(0)
#define K_LET   NUMBER(2)
#define K_BEGIN NUMBER(4)
#define OP_RUNC NUMBER(12)

    static const void *op[] = {
        &&k_halt,   // 0
        &&op_let,   // 1
        &&k_let,    // 2
        &&op_begin, // 3
        &&k_begin,  // 4
        &&op_quote, // 5
        &&op_set,   // 6
        &&op_app,   // 7
        &&op_if,    // 8
        &&op_letcc, // 9
        &&op_prim,  // 10
        &&op_ref,   // 11
        &&op_runc,  // 12
    };


/* A continuation frame contains a code tag, the environment and an
   arbitrary argument. */
#define PUSHK(tag, arg) PUSH(k, CONS(tag, CONS(e, arg)));

    /* If we start with an empty contination, push an explicit halt
       frame to the k stack. */
    if (MT == k) {
        PUSHK(K_HALT, NIL);
    }
    
  c_reduce:
    /* Reduce core form. */
    i     = NCAR(c);     // get opcode
    arg   = CDR(c);      // get op arg
    goto run;

  k_return:
    /* Execute top continuation frame, passing it the return value
       (stored in the c register). */
    arg  = POP(k);      // pop k stack
    i    = NPOP(arg);   // pop opcode
    e    = CAR(arg);    // restore env

  run:
    // DISP("run %d\n", i);
    goto *op[i];

  op_let: /* (exp1 . exp2) */
    PUSHK(K_LET, CDR(arg));
    c = CAR(arg);
    goto c_reduce;
  k_let: /* (e . exp2) */
    // e = CONS(c, CAR(c));  // update env w. value
    PUSH(e, CAR(c));
    c = CDR(arg);
    goto c_reduce;

  op_begin: /* (exp1 . exp2) */
    PUSHK(K_BEGIN, CDR(arg));
    c = CAR(arg);
    goto c_reduce;
  k_begin: /* (e . exp2) */
    // e = CAR(arg); // don't update env
    c = CDR(arg);
    goto c_reduce;

  op_quote: /* datum */
    c = arg;
    goto k_return;

  op_set: /* (var_dst . var_src) */
    e_set(e, NCAR(arg), e_ref(e, NCDR(arg)));
    c = VOID;
    goto k_return;

  op_app: /* ((env . (nr . expr)) . v_args) */

    /* Pop closure info into machine registers. */
    c   = CAR(arg); // closure
    arg = CDR(arg); // argument list (varrefs)
    e   = POP(c);   // pop e=env
    i   = NPOP(c);  // pop i=nr, c=expr

    /* nr =  number of arguments + a "rest" flag shifted in LSB. */
    e_ext = e;
    /* Ref args and extend environment. */
    while (i>>1) {
        PUSH(e_ext, e_ref(e, NPOP(arg)));
        i -= 2;
    }
    /* Push rest args. */
    if (i) { e_ext = CONS(arg, e_ext); }
    e = e_ext;
    e_ext = NIL;  // kill ref for gc
    goto c_reduce;

  op_if: /* (var . (exp_t . exp_f)) */
    c = CDR(e_slot(e, NCDR(arg)));
    arg = CDR(arg);
    c = (c != FALSE) ? CAR(arg) : CDR(arg);
    goto c_reduce;

  op_ref:   /* number */
    i = (arg - heap) & 0xFF;   // FIXME: this is ugly: CAR to early
    c = e_ref(e, i);
    goto k_return;
        
  op_prim:  /* atom */
    ((vm_prim)arg->atom)(vm);
    goto k_return;

    /* Reification of continuations takes a detour: 

       First op_letcc wraps the continuation as a closure over an
       empty environment and a op_runc primitive.  This ensures that
       the continuation object can be applied to a value.

       After such application this value ends up as the only element
       in the environment accessible by the op_runc opcode.  The k
       stack is passed as an argument to op_runc. */
  op_letcc: /* () */
    c = CONS(NIL, CONS(NUMBER(2), CONS(OP_RUNC, k)));
    goto k_return;
  op_runc:  /* k */
    c = CAR(e);
    k = arg;
    e = NIL;
    goto k_return;


  k_halt:
    /* Clear temps before returning. */
    arg   = NIL;
    e_ext = NIL;
    return;

#undef c
#undef e
#undef k
#undef arg
#undef e_ext
}


cell *vm_eval(vm *vm, cell *expr) {
    vm->c = expr;
    vm->e = NIL;
    vm->k = MT;
    vm_continue(vm);
    return vm->c;
}






#if 1
#define TEST(expr) { test(&vm, expr); }
void test(vm *vm, cell *expr) {
    DISP("in:  "); cell_display(expr); newline();
    DISP("out: "); cell_display(vm_eval(vm, expr)); newline();
}

void test_prim(vm *vm) {
    // DISP("test_prim\n");
    vm->c = VOID;
}

int main(void) {

    /* init VM + GC */
    vm vm = VM_INIT;
    heap_clear();
    heap_set_roots((cell**)&vm);


    cell *atom = ATOM((void*)0xF00F000);
    cell *prim = ATOM((void*)&test_prim);
    while (1) {
#define OP(n,x) CONS(NUMBER(n),x)
        cell *q, *p;
        TEST (OP(5, prim));       // (quote atom)
        TEST (q = OP(5, atom));   // (quote atom)
        TEST (p = OP(10, prim));  // (prim  atom)
        TEST (OP(1, CONS(q, OP(11, NUMBER(0))))); // (let (var atom) var)
        TEST (OP(3, CONS(q, q)));
     

        {
            /* Apply closure: (let ((v0 123)) (lambda () v0)) */            
            cell *env  = CONS(NUMBER(123), NIL);
            cell *body = OP(11, NUMBER(0)); // (ref v0)
            cell *closure = CONS(env, CONS(NUMBER(0), body));
            TEST (OP(7, CONS(closure, NIL)));
        }

        {
            /* Apply closure: (let ((v1 234) (v0 123)) (lambda () v1)) */
            cell *env  = CONS(NUMBER(123), CONS(NUMBER(234), NIL));
            cell *body = OP(11, NUMBER(1)); // (ref v1)
            cell *closure = CONS(env, CONS(NUMBER(0), body));
            TEST (OP(7, CONS(closure, NIL)));
        }
    }
    
    return 0;
}
#endif
