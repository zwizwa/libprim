#include <leaf/tuple.h>
#include <leaf/port.h>
#include <stdlib.h>

static void tuple_free(tuple *x) {
    int i;
    for (i=0; i<x->size; i++) {
        leaf_object *o = x->slot[i];
        if (o) leaf_free(o);
        x->slot[i] = NULL;
    }
    free(x);
}

#define LP "("
#define RP ")"

static int tuple_write(tuple *x, port *p) {
    int i, num = 0;
    num += port_printf(p, LP);
    for (i=0; i<x->size; i++) {
        if (i) { num += 1; port_putc(p, ' '); }
        if (x->slot[i]) {
            num += leaf_write(x->slot[i], p);
        }
        else {
            num += port_printf(p, "#<null>");
        }
    }
    num += port_printf(p, RP);
    return num;
}

static tuple_class *type = NULL;
leaf_class *tuple_type(void) {
    if (!type) {
        type = calloc(1, sizeof(*type));
        type->super.free  = (leaf_free_m)tuple_free;
        type->super.write = (leaf_write_m)tuple_write;
    }
    return (leaf_class*)type;
}

tuple *tuple_new(int size) {
    tuple *t = calloc(1, sizeof(*t) + sizeof(leaf_object*) * size);
    t->base.type = tuple_type();
    t->size = size;
    return t;
}
