#include <stdio.h>
#include <stdlib.h>
#include "scheme.h"

atom_class dummy_op = {free};

void debug_gc(gc *gc) {
    atom *a = malloc(sizeof(*a));
    a->op = &dummy_op;
    object_vec_set(gc_alloc(gc, 1), 0, (object)a);
    object_vec_set(gc->roots, 0, gc_alloc(gc, 1));
    // gc_collect(gc);
}

void debug_scheme(sc *sc) {
    object o = make_state(sc, 0, 0, 0, 0);
    state *s = object_state(o);
    
}

int main(int argc, char **argv) {
    gc *gc = gc_new(20);
    sc *sc = scheme_new();
    for(;;) debug_gc(gc);
    for(;;) debug_scheme(sc);
}

