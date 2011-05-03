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


/* Code implementation.
 
   In the original oplementation C is CONS-cell based data structure.
   It behaves mostly as a stream but is implemented as a CDR-linked
   list with some improper list optimizations sprinkled in.

   The interesting part is that if access is made abstract,
   representation can be made abstract too and code representation can
   be optimized to other specifications.
*/

/* Read a number from the code stream. */
#define NEXT_NUM  NPOP(c) 

/* Read a structured data item from the stream: try to avoid this as
   it hinders abstraction.  Replace with other typed reads.  */
#define NEXT_ADDR   POP(c)         // read abstract code address
#define NEXT_DATA   POP(c)         // read constant data
#define NEXT_VOID   (POP(c)->atom) // read full machine address

/* VM main loop.

   The basic idea is the same as in most other libprim VMs.

   The machine has the following registers:

   C: the current code to be interpreted
   E: the current variable binding environment (CDR linked list).
   K: the current continuation, (CDR linked stack of frames).
   V: value result of last reduction
   T1,T2: temps

   There is only one continuation type, which corresponds to the
   return point of the evaluation of a one-variable `let' expression.
   This allows continuations to be untagged (as opposed to the other
   libprim VM).

*/

union word {
    vm_prim p;
    int i;
};


#define CLEAR(r) r = VOID  // remove GC references.

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

    /* Non-managed integer temp registers. */
    union word w1, w2;

    /* Opcodes encoded as NUMBER(). */
    #define OP_HALT  0
    #define OP_RUNC 12

    static const void *op[] = {
        &&op_halt,  // 0
        &&op_call,  // 1
        &&op_close, // 2
        &&op_runc,  // 3
        &&op_drop,  // 4
        &&op_quote, // 5
        &&op_set,   // 6
        &&op_app,   // 7
        &&op_if,    // 8
        &&op_letcc, // 9
        &&op_prim,  // 10
        &&op_ref,   // 11

        /* Debug */
        &&op_dump,  // 12
    };


    if (MT == k) {
        /* If we start with an empty contination, push an explicit
           halt frame to the k stack.  If `exp' is the code to
           evaluate, this behaves as (let ((v exp)) (halt v)). */
        t1 = CONS(NUMBER(OP_HALT), 
                  NIL); // dummy, not reached
        PUSH(c, t1);
        CLEAR(t1);
        goto op_call;
    }
    else {
        /* Otherwise assume proper continuation and run VM. */
        goto c_reduce;
    }
    
  c_reduce:
    /* Reduce current core form; interpreter opcode is in CAR. */
    w1.i = NEXT_NUM; // opcode
    // DISP("{%d}", w1.i);
    goto *op[w1.i]; 

  op_call: /* (exp_later . exp_now) */
    /* Code sequencing: take some code to evaluate later in the
       extended environment, and some code to evaluate now with the
       purpose of storing the value it produces in the environment. */

    /* Push closure for exp_later to continuation stack. */
    t1 = CONS(e, NEXT_ADDR); // (e . c) == closure
    PUSH(k, t1);             // k -> ((e . c) . k)
    CLEAR(t1);
    goto c_reduce;

  k_return: /* Pass value in v to continuation. */
    /* Pop closure */
    t1 = POP(k);
    e = CAR(t1);
    c = CDR(t1);
    CLEAR(t1);
    /* Extend restored environment with the return value
       of a previous evaluation. */
    PUSH(e, v);
    CLEAR(v);
    goto c_reduce;

  op_drop: /* exp */
    /* Ignore top element in the environment and continue reducing.
       Used to implement `begin' which ignores return values of first
       expression, in terms of op_call, which saves return value in
       the environment. */
    POP(e);
    goto c_reduce;

  op_quote: /* datum */
    v = NEXT_DATA;
    goto k_return;

  op_set: /* (var_dst . var_src) */
    w1.i = NEXT_NUM;
    w2.i = NEXT_NUM;
    e_set(e, w1.i, e_ref(e, w2.i));
    v = VOID;
    goto k_return;

  op_app:  /* (v_fn . v_cs)
               v_fn -> ((env . (nr . expr)) . v_cs) */

    // FIXME:  No type checking.  Crashes if v_fn doesn't point to closure.

    /* Name temp regs. */
    #define closure t2
    #define e_ext   t1
    #define dotarg  v

    w1.i = NEXT_NUM;             // var -> closure
    closure = e_ref(e, w1.i);    // (env . (nr . expr))
    e_ext = POP(closure);        // new env to extend
    w1.i = NPOP(closure);        // (nb_args << 1) | rest_args

    /* Ref args from current env and extend new env. */
    while (w1.i>>1) {
        PUSH(e_ext, e_ref(e, NEXT_NUM));
        w1.i -= 2;
    }
    /* Ref rest and push as a list if desired. */
    if (w1.i) {
        dotarg = NIL;
        while(NIL != c) { PUSH(dotarg, e_ref(e, NEXT_NUM)); }
        PUSH(e_ext, dotarg);
    }

    c = closure;
    e = e_ext;
    CLEAR(e_ext);
    CLEAR(closure);
    CLEAR(dotarg);
    goto c_reduce;

    /* Name cleanup */
    #undef closure
    #undef e_ext
    #undef dotarg
    

  op_close: /* (nr . expr) */
    v = CONS(e, c);
    goto k_return;

  op_if: /* (var . (exp_t . exp_f)) */
    v = e_ref(e, NEXT_NUM);
    t1 = NEXT_ADDR;
    if (v == FALSE) { t1 = NEXT_ADDR; }
    c = t1;
    CLEAR(v);
    CLEAR(t1);
    goto c_reduce;

  op_ref:   /* number */
    v = e_ref(e, NEXT_NUM);
    goto k_return;

  op_dump: /* var */
    v = e_ref(e, NEXT_NUM);
    DISP("[op_dump: ");
    cell_display(v);
    DISP("] ");
    v = VOID;
    goto k_return;
        
  op_prim:  /* atom */
    v = VOID; // in case prim doesn't set it
    w1.p = NEXT_VOID;
    w1.p(vm);
    goto k_return;

    /* Reification of continuations takes a detour: 

       First op_letcc wraps the continuation as a closure over an
       empty environment and a op_runc primitive.  This ensures that
       the continuation object can be applied to a value.

       After such application this value ends up as the only element
       in the environment accessible by the op_runc opcode.  The k
       stack is passed as an argument to op_runc. */
  op_letcc: /* () */
    // FIXME: check this
    t1 = CONS(NUMBER(OP_RUNC), k);
    t1 = CONS(NUMBER(2), t1);
    v = CONS(NIL, t1);
    CLEAR(t1);
    goto k_return;
  op_runc:  /* k */
    v = POP(e);
    k = c;
    goto k_return;


  op_halt:
    /* Halt use a fake let continuation, which pushes the retval to
       the environment stack. */
    v = POP(e);
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




