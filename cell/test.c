
#include <cell/vm.h>
#include <cell/read.h>

#if 1
#define TEST(expr) { test(&vm, expr); }


cell *test(vm *vm, cell *expr) {
    cell *rv;
    DISP("in:  "); cell_display(expr); newline();
    DISP("out: "); cell_display(rv = vm_eval(vm, expr)); newline();
    return rv;
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

#if 0
void looptest(vm *vm) {
    heap_collect();
    cell *c1 = CONS(NUMBER(0), VOID);
    cell *closure = CONS(NIL, c1);
    cell *op_dummy = OP(5, NUMBER(123));
    cell *expr = OP(3, CONS(op_dummy, OP(7, CONS(closure, NIL))));
    SET_CDR(c1, expr);
    vm_eval(vm, expr);

}
#endif



int readtest(vm *vm) {

    // FILE *f = fopen("/dev/ser2", "a+");
    // port *p = port_file_new(f, "stdin");
    port *p = port_file_new(stdin, "<stdin>");


    int rv = 0;
    while(1) {
        heap_collect();
        cell *expr = vm_read(vm, p);
        
        if (EOF_OBJECT == expr) return rv;
        

        if (0) {
            cell_display(expr);
            newline();
        }
        else {
            if (NUMBER(123) != test(vm, expr)) {
                DISP("FAIL!\n");
                rv++;
            }
        }
    }
}

// #define ECOS
#ifdef ECOS
/* Need the scheduler to use IO in current config. */
#include <cyg/infra/diag.h>
#include <cyg/kernel/kapi.h>
void app_start(cyg_addrword_t data);
cyg_uint32 app_stack[1024*2];
static cyg_thread app_thread;
static cyg_handle_t app_handle;

void cyg_user_start(void)
{
    diag_printf("libprim eCos CELL test\n");
    cyg_thread_create(0, app_start, 0, "ecos_cell",
                      app_stack, sizeof(app_stack),
                      &app_handle, &app_thread);

    /* After this function exits, the scheduler starts running resumed
       threads. */
    cyg_thread_resume(app_handle);
}
void app_start(cyg_addrword_t data)

#else
int main(void) 
#endif
{
    printf("CELL test\n");

    // while(1) { putchar(getchar()); }

    /* init VM + GC */
    vm vm = VM_INIT;
    heap_clear();
    heap_set_roots((cell**)&vm);

    // looptest(&vm);
    // while (1) { tests(&vm); }
    // return 0;
    readtest(&vm);
}

#endif
