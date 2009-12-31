// Hack it on top of SC

#define SC_NEW_VM
#include <sc/scheme.c>
#include <mz/vm.h_prims>

static prim_def vm_prims[] = vm_table_init;

_ sc_vm(sc *sc, _ c, _ e, _ k) {
    sc->c = c;
    sc->e = e;
    sc->k = k;

    // piggyback on mother vm for exceptions & abort
    for(;;) {
        /* Run next opcode.  
           
           Machine primitives are implemented using the same data type
           as C primitives.

           Note however that primitives that occur in the instruction
           stream need to modify the machine state in-place: their
           return value is ignored.  An ordinary primtive that doesn't
           touch sc->c will cause an infinite loop.
        */
        prim *p = CAST(prim, CAR(sc->c));
        _ args  = CDR(sc->c);
        _sc_call(sc, prim_fn(p), prim_nargs(p), args);
    }
}

int main(int argc, char **argv) {
    sc *sc = _sc_new(argc, (const char**)argv);
    _sc_def_prims(sc, vm_prims);
    _sc_continue(sc);
    return 0;
}
