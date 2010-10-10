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

#include <cell/vm.h>

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

    /* Registers are in the vm struct which is set as GC root.  Care
       needs to be taken that cell refs never go into local variables
       only.  This includes expression intermediates!  

       Meaning: the arguments of cons need to refer to values that are
       referenced in registers, and the cell pointer goes into a
       register. */

    #define c  (vm->c)   // expression to reduce
    #define e  (vm->e)   // lexical environment
    #define k  (vm->k)   // execution context
    #define v  (vm->v)   // value register
    #define t1 (vm->t1)  // temp reg
    #define t2 (vm->t2)  // temp reg

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
        &&op_close, // 13
    };


    /* A continuation frame is an improper list (r e . c)
       - r : machine code return address
       - e : environment
       - c : code body */
    #define PUSHK(r, c) {    \
            t1 = CONS(e, c); \
            PUSH(t1, r);     \
            PUSH(k, t1);     \
            t1 = VOID; }
    
    /* If we start with an empty contination, push an explicit halt
       frame to the k stack. */
    if (MT == k) {
        PUSHK(K_HALT, NIL);
    }
    
  c_reduce:
    /* Reduce core form. */
    i = NPOP(c); // opcode
    goto run;

  k_return:
    /* Execute top continuation frame, passing it the return value
       (stored in the c register). */
    c = POP(k);  // k frame
    i = NPOP(c); // opcode
    e = POP(c);  // environment
    goto run;

  run:
    goto *op[i];

  op_let: /* (exp_later . exp_now) */
    PUSHK(K_LET, POP(c));
    goto c_reduce;
  k_let: /* exp_later */
    PUSH(e, v);  // store value in environment, exp_later is already in c.
    v = VOID;
    goto c_reduce;

  op_begin: /* (exp_later . exp_now) */
    PUSHK(K_BEGIN, POP(c));
    goto c_reduce;
  k_begin: /* exp_later */
    v = VOID; // exp_later is alredy in c and we ignore the value.
    goto c_reduce;

  op_quote: /* datum */
    v = c;
    goto k_return;

  op_set: /* (var_dst . var_src) */
    e_set(e, NCAR(c), e_ref(e, NCDR(c)));
    v = VOID;
    goto k_return;

  op_app: /* ((env . (nr . expr)) . v_cs) */

    /* Name temp regs. */
    #define closure t2
    #define e_ext   t1
    #define dotarg  v

    closure = POP(c);      // (env . (nr . expr))
    e_ext = POP(closure);  // new env to extend
    i = NPOP(closure);     // (nb_args << 1) | rest_args

    /* Ref args from current env and extend new env. */
    while (i>>1) {
        PUSH(e_ext, e_ref(e, NPOP(c)));
        i -= 2;
    }
    /* Ref rest and push as a list if desired. */
    if (i) {
        /* Push them onto v first. */
        dotarg = NIL;
        while(NIL != c) { PUSH(dotarg, e_ref(e, NPOP(c))); }
        PUSH(e_ext, dotarg);
    }

    c = closure;
    e = e_ext;
    e_ext = closure = dotarg = VOID;  // kill refs for gc
    goto c_reduce;

    /* Name cleanup */
    #undef closure
    #undef e_ext
    #undef dotarg

  op_close: /* (nr . expr) */
    v = CONS(e, c);
    goto k_return;

  op_if: /* (var . (exp_t . exp_f)) */
    v = e_ref(e, NPOP(c));
    c = (v != FALSE) ? CAR(c) : CDR(c);
    v = VOID;
    goto c_reduce;

  op_ref:   /* number */
    i = NCELL(c);
    v = e_ref(e, i);
    goto k_return;
        
  op_prim:  /* atom */
    ((vm_prim)c->atom)(vm);
    goto k_return;

    /* Reification of continuations takes a detour: 

       First op_letcc wraps the continuation as a closure over an
       empty environment and a op_runc primitive.  This ensures that
       the continuation object can be applied to a value.

       After such application this value ends up as the only element
       in the environment accessible by the op_runc opcode.  The k
       stack is passed as an argument to op_runc. */
  op_letcc: /* () */
    v = CONS(NIL, CONS(NUMBER(2), CONS(OP_RUNC, k)));
    goto k_return;
  op_runc:  /* k */
    v = POP(e);
    k = c;
    goto k_return;


  k_halt:
    /* Clear temps before returning. */
    return;

    #undef c
    #undef e
    #undef k
    #undef v
    #undef t1
    #undef t2
}


cell *vm_eval(vm *vm, cell *expr) {
    vm->c = expr;
    vm->e = NIL;
    vm->k = MT;
    vm_continue(vm);
    return vm->v;
}




