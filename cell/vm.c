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

#define CAR(cell) car(*(cell))
#define CDR(cell) cdr(*(cell))
#define SET_CAR(c, v) heap_set_cons(c, v, CDR(c))
#define VOID NIL
#define CONS(a,b) heap_cons(a,b)

/* Fetch an opcode from a cell.  We repurpose an unused part of the
   address space for this.  During interpretation only low bits are
   kept. */
#define NCAR(x) (icar(x) & 0xFF)
#define NCDR(x) (icdr(x) & 0xFF)


typedef struct void (*opcode)(vm *vm);

static opcode opcodes[];

/* Pop top k frame, return args. */
cell* k_pop_arg(vm *vm) {
    cell *k_top = CAR(vm->k);
    vm->k = CDR(vm->k);
    return CDR(k_top);
}

/* Get environment slot. */
cell* e_slot(vm *vm, int i) {
    cell *s = vm->e; 
    while(i--) s = CDR(s);
    return s;
}

/* Invoke code tag in top k frame, don't pop.  Value is in vm->c. */
void vm_return(vm *vm) { opcodes[NCAR(CAR(vm->k))](vm); }

/* Interpret next code expression in vm->c */
void vm_next(vm *vm)   { opcodes[NCAR(vm->c)](vm); }


#define OP_QUOTE 0
// ( 0 . datum )
void op_quote(vm *vm) { 
    vm->c = CDR(vm->c);
    vm_return(vm); 
}

#define OP_SET 1
// ( 1 . ( var . var ) )
void op_set(vm *vm) {
    cell *dst = e_slot(vm, NCAR(vm->c));
    cell *src = e_slot(vm, NCDR(vm->c));
    SET_CAR(dst, CAR(src));
    vm_return(VOID);
}

#define OP_LET 2
#define K_LET 3
// ( 2 . ( expr . expr ))
void op_let(vm *vm) {
    cell *ee = CDR(vm->c);
    vm->c = CAR(ee);
    cell *kf = CONS(heap_number(K_LET), CDR(ee));
    vm->k = CONS(kf, vm->k);
}
// ( 3 . expr )
void k_let(vm *vm) {
    vm->env = CONS(vm->c, vm->env);
    vm->c = k_pop_arg(vm);
}



static opcode opcodes[] = {
    op_quote, // 0
    op_set,   // 1
    op_let,   // 2
    k_let,    // 3
};




