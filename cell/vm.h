#ifndef _CELL_VM_H_
#define _CELL_VM_H_

#include <cell/gc.h>


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



struct _vm;
typedef struct _vm vm;
typedef void (*vm_prim)(vm *vm);

union word {
    vm_prim p;
    int i;
};

struct _vm {
    cell *c;   // code pointer (IP)
    cell *e;   // lexical environment
    cell *k;   // continuation
    cell *v;   // return value
    cell *t1;
    cell *t2;

    void *end_gc; // end marker for GC root

    /* Non-managed integer temp registers. */
    union word wreg1;
    union word wreg2;

} __attribute((__packed__));
#define VM_INIT {VOID,VOID,VOID,VOID,VOID,VOID,0,0,0}



void vm_continue(vm *vm);
cell *vm_eval(vm *vm, cell *expr);

#endif


