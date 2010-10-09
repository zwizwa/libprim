#ifndef _CELL_VM_H_
#define _CELL_VM_H_

#include "cellgc.h"

struct _vm {
    cell *c;
    cell *e;
    cell *k;
    cell *v;
    cell *t1;
    cell *t2;
    void *END;
} __attribute((__packed__));
#define VM_INIT {VOID,VOID,VOID,VOID,VOID,VOID,0};

typedef struct _vm vm;

typedef void (*vm_prim)(vm *vm);


void vm_continue(vm *vm);
cell *vm_eval(vm *vm, cell *expr);

#endif
