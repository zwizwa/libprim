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
    cell *a;
    cell *t;
    void *END;
} __attribute((__packed__));
typedef struct _vm vm;


/* Get environment slot and ref. */
cell* e_slot(cell *e, int i) {
    while(i--) e = CDR(e);
    return e;
}
cell* e_ref(cell *e, int i) {
    return CDR(e_ref(e, i));
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

#define c     (vm->c)   // expression
#define e     (vm->e)   // lexical environment
#define k     (vm->k)   // execution context
#define arg   (vm->a)   // argument to op_ or k_
#define e_ext (vm->t)   // temp env

#define K_HALT  NUMBER(0)
#define K_LET   NUMBER(2)
#define K_BEGIN NUMBER(3)

    static void *op[] = {
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
    };

    /* If we start with an empty contination, push an explicit halt
       frame to the k stack. */
    if (MT == k) {
        PUSH(k, CONS(K_HALT, NIL));
    }
    
  c_reduce:
    /* Reduce core form. */
    i     = NCAR(c);     // get opcode
    arg   = CDR(c);      // get op arg
    goto run;
  k_return:
    /* Execute top continuation frame, passing it the return value
       (stored in the c register). */
    arg  = CAR(k);
    i    = NCAR(arg);   // get opcode
    arg  = CDR(arg);    // get k args
    k    = CDR(k);      // pop k stack

  run:
    DISP("run %d\n", i);
    goto *op[i];

  op_let: /* (exp1 . exp2) */
    PUSH(k, CONS(K_LET, CONS(e, CDR(arg))));
    c = CDR(arg);
    goto c_reduce;
  k_let: /* (e . exp2) */
    e = CONS(c, CAR(arg));  // update env w. value
    c = CDR(arg);
    goto c_reduce;

  op_begin: /* (exp1 . exp2) */
    PUSH(k, CONS(K_BEGIN, CONS(e, CDR(arg))));
    c = CAR(arg);
    goto c_reduce;
  k_begin: /* (e . exp2) */
    e = CAR(arg); // don't update env
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

  op_letcc: /* () */
    c = k;
    goto k_return;

  op_prim:  /* atom */
    ((vm_prim)arg)(vm);
    goto k_return;

  k_halt:
    return;
#undef c
#undef e
#undef k
#undef arg
#undef e_ext
}


cell *eval(cell *expr) {
    vm vm = {
        .c   = expr, 
        .e   = NIL, 
        .k   = MT, 
        .a   = NIL,
        .t   = NIL,
        .END = 0
    };
    heap_set_roots((cell**)&vm);
    vm_continue(&vm);
    return vm.c;
}

#if 0
#define TEST(expr) {heap_collect(); test(expr); } // GC before CONS
void test(cell *expr) {
    DISP("in:  "); cell_display(expr); newline();
    DISP("out: "); cell_display(eval(expr)); newline();
}
int main(void) {
    heap_clear();

    while (1) {
        TEST (CONS(NUMBER(5), ATOM((void*)0xF00F000)));  // (quote atom)
    }
    
    return 0;
}
#endif
