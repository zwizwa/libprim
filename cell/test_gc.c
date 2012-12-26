#include <cell/gc.h>
void heap_clear(void);

int main(void) {
    heap_clear();
    cell *x = NIL;

    // cell *atom = heap_atom((void*)0xDEADBEEF);
    // cell *circ = heap_cons(NIL, NIL);
    // circ->pair = cons_tag(TAG_MARKED, atom, circ);
    // x = circ;

    x = CONS(VOID, x);
    x = CONS(TRUE, x);
    x = CONS(FALSE, x);

    x = CONS(NUMBER(3), x);
    x = CONS(NUMBER(2), x);
    x = CONS(NUMBER(1), x);

    cell *_roots[] = {x, NULL};

    heap_set_roots(_roots);

    for(;;) {
        // cell_display(root); newline();
        CONS(NIL, NIL);
        printf("used: %d\n", heap_used());
    }
    return 0;
}
