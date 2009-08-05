#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"
#include "gc.h"
#include <setjmp.h>

void free_dummy(void *x){
    fprintf(stderr, "free_dummy(%p)\n", x);
    free(x);
}
atom_class dummy_op = {free_dummy};

typedef struct {
    gc *gc;
    object root;
} gc_test;


void debug_gc(gc_test *x) {
    for (;;) {
        atom *a = malloc(sizeof(*a));
        a->op = &dummy_op;
        object o = gc_vector(x->gc, 1, atom_to_object(a));
        x->root = gc_vector(x->gc, 1, o);
        // gc_collect(x->gc);
    }

    // gc_collect(gc);
}

object sc_interpreter_step(sc*, object);
object sc_make_state(sc*, object, object);

void debug_scheme(sc *sc) {
    for (;;) {
        object e = NIL;
        object t = CONS(SYMBOL("null?"), CONS(NIL, NIL));
        object c = CLOSURE(t,e);
        object k = NIL;
        object s = STATE(c,k);
        for(;;) {
            sc_write(sc, s);
            printf("\n");
            s = sc_interpreter_step(sc, s);
        }
    }
}

void mark_roots(gc_test *x){
    x->root = gc_mark(x->gc, x->root);
    fprintf(stderr, "gc - reduced to %ld slots\n", x->gc->current_index);
}

int main(int argc, char **argv) {
    gc_test *x = malloc(sizeof(*x));
    x->gc = gc_new(20, (gc_mark_roots)mark_roots, x);
    x->root = integer_to_object(0);
    // debug_gc(x);
    debug_scheme(scheme_new());
    return 0;
}

