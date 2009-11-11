#include <leaf/tree.h>
#include <stdlib.h>

static tree_class *type = NULL;

static void tree_free(tree *x) {
    int i;
    for (i=0; i<x->size; i++) {
        free_leaf((leaf_object*)(x->slot + i));
        x->slot[i] = NULL;
    }
    free(x);
}

tree *tree_new(int size) {
    if (!type) {
        type = calloc(1, sizeof(*type));
        type->super.free = (leaf_free)tree_free;
    }
    tree *t = calloc(1, sizeof(*t) + sizeof(leaf_object*) * size);
    t->type = type;
    return t;
}
