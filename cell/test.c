
#include "cellvm.h"
#include "cellread.h"

#if 1
#define TEST(expr) { test(&vm, expr); }


void test(vm *vm, cell *expr) {
    DISP("in:  "); cell_display(expr); newline();
    DISP("out: "); cell_display(vm_eval(vm, expr)); newline();
}

void test_prim(vm *vm) {
    // DISP("test_prim\n");
    vm->v = VOID;
}


void tests(vm *vm) {
    
    /* Need to manually call GC here to try to prevent it from being
       called with cell refs in local vars. */
    heap_collect();

#define atom ATOM((void*)0xF00F00)
#define prim ATOM((void*)&test_prim)
#define OP(n,x) CONS(NUMBER(n),x)
    cell *q, *p;

    heap_collect();
    test(vm, OP(5, prim));       // (quote atom)
    test(vm, q = OP(5, atom));   // (quote atom)
    test(vm, p = OP(10, prim));  // (prim  atom)
    test(vm, OP(1, CONS(q, OP(11, NUMBER(0))))); // (let (var atom) var)
    test(vm, OP(3, CONS(q, q)));
    

    heap_collect();
    {
        /* Apply closure: (let ((v0 123)) (lambda () v0)) */            
        cell *env  = CONS(NUMBER(123), NIL);
        cell *body = OP(11, NUMBER(0)); // (ref v0)
        cell *closure = CONS(env, CONS(NUMBER(0), body));
        test(vm, OP(7, CONS(closure, NIL)));
    }

    heap_collect();
    {
        /* Apply closure: (let ((v1 234) (v0 123)) (lambda () v1)) */
        cell *env  = CONS(NUMBER(123), CONS(NUMBER(234), NIL));
        cell *body = OP(11, NUMBER(1)); // (ref v1)
        cell *closure = CONS(env, CONS(NUMBER(0), body));
        test(vm, OP(7, CONS(closure, NIL)));
    }
    
}

void looptest(vm *vm) {
    heap_collect();
    cell *c1 = CONS(NUMBER(0), VOID);
    cell *closure = CONS(NIL, c1);
    cell *op_dummy = OP(5, NUMBER(123));
    cell *expr = OP(3, CONS(op_dummy, OP(7, CONS(closure, NIL))));
    SET_CDR(c1, expr);
    vm_eval(vm, expr);

}

void readtest(vm *vm) {
    while(1) {
        heap_collect();
        cell *expr = vm_read_stdin(vm);
        test(vm, expr);
    }
}

int main(void) {

    /* init VM + GC */
    vm vm = VM_INIT;
    heap_clear();
    heap_set_roots((cell**)&vm);

    // looptest(&vm);
    // while (1) { tests(&vm); }
    readtest(&vm);
    return 0;
}

#endif
