#include <leaf/leaf.h>
#include <leaf/port.h>

void leaf_free(leaf_object *x) {
    x->type->free(x);
}
int leaf_write(leaf_object *x, port *p) {
    if (x->type->write) {
        return x->type->write(x, p);
    }
    else {
        return port_printf(p, "#<leaf:%p>");
    }
}
