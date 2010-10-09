/* VM on top of CELL.  

   The main idea behind the CELL graph memory is to run on ARM cores
   with little memory (around 32-128 kB).

   For the interpreter it would be convenient to:
     - have compact byte code
     - run code in graph memory
     - run code in flat (constant) Flash memory

   The core language is ANF[1], possibly written in `let*' form which
   collapses nested `let' in one form.


   EXP ::= (app VAL VAL ...)
        |  (let (VAR EXP) EXP)
        |  (begin EXP EXP)
        |  (set! VAR VAL)
        |  (if VAL EXP EXP)

   VAL ::= (lambda (VAR ...) EXP)
        |  VAR

   [1] http://en.wikipedia.org/wiki/Administrative_normal_form

*/

#include "cell.h"

typedef struct {
    cell *c;
    cell *e;
    cell *k;
} vm;


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

typedef void* op;
struct opcodes { 
    op op_halt;
    op op_let;    op k_let;
    op op_begin;  op k_begin;
    op op_quote;
    op op_set;
    op op_app;
    op op_if;
    op op_letcc;
} __attribute((__packed__));;
#define OP(name)                                                    \
    ({struct opcodes opc;                                           \
        heap_number((&(opc.name)-&(opc.op_halt)) / sizeof(op)); })

void vm_continue(vm *vm) {
    cell *c = vm->c;
    cell *e = vm->e;
    cell *k = vm->k;

    cell *arg;   // argument to op_ or k_
    int i;       // current opcode / variable index
    cell *e_ext; // temp env var

    static struct opcodes op = {
        .op_halt  = &&op_halt, 
        .op_let   = &&op_let, 
        .k_let    = &&k_let, 
        .op_begin = &&op_begin, 
        .k_begin  = &&k_begin,
        .op_quote = &&op_quote,
        .op_set   = &&op_set, 
        .op_app   = &&op_app, 
        .op_if    = &&op_if, 
        .op_quote = &&op_quote,
        .op_letcc = &&op_letcc,
    };

    
  c_reduce:
    /* Reduce current expression. */
    i    = NCAR(c);     // get opcode
    arg  = CDR(c);      // get op arg
    goto run;
  k_return:
    /* Execute top continuation frame, passing return value stored in
       the c register. */
    arg  = CAR(k);
    i    = NCAR(arg);   // get opcode
    arg  = CDR(arg);    // get k args
    k    = CDR(k);      // pop k stack

  run:
    goto **(((void **)(&op))+i);

  op_let: /* (exp1 . exp2) */
    PUSH(k, CONS(OP(k_let), CONS(e, CDR(arg))));
    c = CDR(arg);
    goto c_reduce;
  k_let: /* (e . exp2) */
    e = CONS(c, CAR(arg));  // update env w. value
    c = CDR(arg);
    goto c_reduce;

  op_begin: /* (exp1 . exp2) */
    PUSH(k, CONS(OP(k_begin), CONS(e, CDR(arg))));
    c = CAR(arg);
    goto c_reduce;
  k_begin: /* (e . exp2) */
    e = CAR(arg); // don't update env
    c = CDR(arg);
    goto c_reduce;

  op_quote: /* datum */
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
    goto c_reduce;

  op_if: /* (var . (exp_t . exp_f)) */
    c = CDR(e_slot(e, NCDR(arg)));
    arg = CDR(arg);
    c = (c != FALSE) ? CAR(arg) : CDR(arg);
    goto c_reduce;

  op_letcc: /* () */
    c = k;
    goto k_return;

  op_halt:
    /* Stop interpreting. */
    vm->c = c;
    vm->e = e;
    vm->k = k;
    return;
}

