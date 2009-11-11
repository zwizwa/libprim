#include <leaf/tree.h>
#include <leaf/port.h>
#include <stdlib.h>

static tree_class *type = NULL;

static void tree_free(tree *x) {
    int i;
    for (i=0; i<x->size; i++) {
        leaf_free((leaf_object*)(x->slot + i));
        x->slot[i] = NULL;
    }
    free(x);
}

static int tree_write(tree *x, port *p) {
    int i, num = 0;
    num += port_printf(p, "{");
    for (i=0; i<x->size; i++) {
        if (i) { num += 1; port_putc(p, ' '); }
        if (x->slot[i]) {
            num += leaf_write(x->slot[i], p);
        }
        else {
            num += port_printf(p, "#<null>");
        }
    }
    num += port_printf(p, "}");
    return num;
}

tree *tree_new(int size) {
    if (!type) {
        type = calloc(1, sizeof(*type));
        type->super.free  = (leaf_free_m)tree_free;
        type->super.write = (leaf_write_m)tree_write;
    }
    tree *t = calloc(1, sizeof(*t) + sizeof(leaf_object*) * size);
    t->type = type;
    t->size = size;
    return t;
}
