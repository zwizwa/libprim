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

void test_prim(sc *sc) {
    for (;;) {
        object t = CONS(SYMBOL("zero?"), 
                        CONS(integer_to_object(123),
                             NIL));
        sc->state = sc_datum_to_state(sc, t);
        _sc_run(sc);
        sc_trap(sc);
    }
}


#include <stdarg.h>
object _sc_list(sc *sc, ...){
    va_list ap;
    object v, l = NIL;
    va_start(ap, sc);
    while((v = va_arg(ap, object))) {
        l = CONS(v,l);
    }
    va_end(ap);
    return sc_reverse(sc, l);
}
#define S SYMBOL
#define I integer_to_object
#define L(...) _sc_list(sc, __VA_ARGS__, 0)

object test_app(sc *sc) {
    for (;;) {
/*         object l = CONS(SYMBOL("lambda"),  */
/*                         CONS(CONS(SYMBOL("abc"), NIL), */
/*                              CONS(integer_to_object(123), NIL))); */
/*         object t = CONS(l, CONS(integer_to_object(456), NIL)); */

        object l = L(S("lambda"),L(S("abc")),I(123));
        object t = L(S("post"), L(l, I(456)));
        object e = _sc_eval(sc, t);
        // sc_post(sc, e);
        sc_trap(sc);
        return e;
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
    test_app(_sc_new());
    return 0;
}

