#include <stdio.h>
#include <stdlib.h>
// #include "scheme.h"
#include "gc.h"

void free_dummy(void *x){
    fprintf(stderr, "free_dummy(%p)\n", x);
    free(x);
}
atom_class dummy_op = {free_dummy};

void debug_gc(gc *gc) {
    for (;;) {
        atom *a = malloc(sizeof(*a));
        a->op = &dummy_op;
        object o  = gc_vector(gc, 1, atom_to_object(a));
        gc->roots = gc_vector(gc, 1, o);
    }

    // gc_collect(gc);
}

//void debug_scheme(sc *sc) {
    // object o = make_state(sc, 0, 0, 0, 0);
    // state *s = object_state(o);
    
//}

void notify(void *x){
    fprintf(stderr, "gc(%p)\n", x);
}

int main(int argc, char **argv) {
    gc *gc = gc_new(20, notify, NULL);
    // sc *sc = scheme_new();
    debug_gc(gc);
    // for(;;) debug_scheme(sc);
    return 0;
}

