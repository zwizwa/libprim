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
    #define K_HALT  0
    #define K_LET   2
    #define K_BEGIN 4
    #define OP_RUNC 12

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
        &&op_dump,  // 14
        &&op_drop,  // 15
    };


    /* A continuation frame is an improper list (e r . c)
       - r : machine code return address
       - e : environment
       - c : code body */
    #define PUSHK(r, c) {                       \
        t1 = CONS(NUMBER(r), c);                \
        PUSH(t1, e);                            \
        PUSH(k, t1);                            \
        t1 = VOID; }

    #define POPK() ({                           \
        c = POP(k);                             \
        e = POP(c);                             \
        NPOP(c);})
        


    /* If we start with an empty contination, push an explicit halt
       frame to the k stack. */
    if (MT == k) {
        PUSHK(K_HALT, NIL);
    }
    
  c_reduce:
    /* Reduce current core form; interpreter opcode is in CAR. */
    goto *op[NPOP(c)]; // opcode

  k_return:
    /* Return from call, restoring environment. */
    goto *op[POPK()];


  op_drop: /* exp */
    /* Ignore top element in the environment and continue reducing.
       Used to implement `begin' in terms of op_let. */
    POP(e);
    goto c_reduce;

  op_let: /* (exp_later . exp_now) */
    PUSHK(K_LET, POP(c));
    goto c_reduce;
  k_let: /* exp_later */
    PUSH(e, v);  // add binding to environment
    v = VOID;
    goto c_reduce;

  op_begin: /* (exp_later . exp_now) */
    PUSHK(K_BEGIN, POP(c));
    goto c_reduce;
  k_begin: /* exp_later */
    v = VOID;  // ignore the value.
    goto c_reduce;

  op_quote: /* datum */
    v = c;
    goto k_return;

  op_set: /* (var_dst . var_src) */
    e_set(e, NCAR(c), e_ref(e, NCDR(c)));
    v = VOID;
    goto k_return;

  op_app: { /* (v_fn . v_cs)
               v_fn -> ((env . (nr . expr)) . v_cs) */

    // FIXME:  No type checking.  Crashes if v_fn doesn't point to closure.

    /* Name temp regs. */
    #define closure t2
    #define e_ext   t1
    #define dotarg  v

    v = POP(c);                    // var: fn
    closure = e_ref(e, NCELL(v));  // (env . (nr . expr))
    e_ext = POP(closure);          // new env to extend
    int i = NPOP(closure);         // (nb_args << 1) | rest_args

    /* Ref args from current env and extend new env. */
    while (i>>1) {
        PUSH(e_ext, e_ref(e, NPOP(c)));
        i -= 2;
    }
    /* Ref rest and push as a list if desired. */
    if (i) {
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
    }

  op_close: /* (nr . expr) */
    v = CONS(e, c);
    goto k_return;

  op_if: /* (var . (exp_t . exp_f)) */
    v = e_ref(e, NPOP(c));
    c = (v != FALSE) ? CAR(c) : CDR(c);
    v = VOID;
    goto c_reduce;

  op_ref:   /* number */
    v = e_ref(e, NCELL(c));
    goto k_return;

  op_dump: /* var */
    v = e_ref(e, NCELL(c));
    DISP("[op_dump: ");
    cell_display(v);
    DISP("] ");
    v = VOID;
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
    v = CONS(NIL, CONS(NUMBER(2), CONS(NUMBER(OP_RUNC), k)));
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




