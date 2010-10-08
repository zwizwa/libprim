/* Structs on top of CELL store (binary trees). 

   The libprim/SC interpreter uses structs, so this is necessary to
   reuse code on top of the CELL store.

   Problem: how to tag CONS cells (the high level ones..) ?
   

*/

#include "cell.h"


#if 0

int main(void) {
    heap_clear();
    cell *x = NIL;

    cell *atom = heap_atom((void*)0xDEADBEEF);
    cell *circ = heap_cons(NIL, NIL);
    heap_set_cons(circ, atom, circ);
    x = circ;

    x = heap_cons(NIL, x);
    x = heap_cons(NIL, x);
    x = heap_cons(NIL, x);
    heap_set_root(x);
    
    for(;;) {
        // cell_display(*root); newline();
        heap_cons(NIL, NIL);
        // printf("used: %d\n", heap_used());
    }
    return 0;
}

#endif

