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
    leaf_init(&t->base, tuple_type());
    t->size = size;
    return t;
}



/* CONS-style stacks/lists using 2-component tuples. */

tuple *tuple_stack_push(tuple *stack, leaf_object *x) {
    tuple* rec = tuple_new(2);
    rec->slot[0] = x;
    rec->slot[1] = (leaf_object*)stack;
    return rec;
}
tuple *tuple_stack_drop(tuple *stack) {
    leaf_free(stack->slot[0]);
    return (tuple*)(stack->slot[1]);
}

tuple *tuple_list_remove(tuple *list, leaf_predicate fn, void *ctx) {
    if (!list) return NULL;
    else if (fn(list->slot[0], ctx)) return tuple_stack_drop(list);
    else {
        tuple *head = list;
        tuple *parent = list;
        while(list) {
            if (fn(list->slot[0], ctx)) {
                parent->slot[1] = (leaf_object*)tuple_stack_drop(list);
                return head;
            }
            parent = list;
            list = (tuple*)(list->slot[1]);
        }
        return head;
    }
}
leaf_object *tuple_list_find(tuple *list, leaf_predicate fn, void *ctx) {
    while (list) {
        if (fn(list->slot[0], ctx)) return list->slot[0];
        list = (tuple*)(list->slot[1]);
    }
    return NULL;
}
