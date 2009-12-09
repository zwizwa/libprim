#include <leaf/leaf.h>
#include <leaf/port.h>

void leaf_free(leaf_object *x) {
    if (x) {
        int rc = leaf_rc(x);
        if (rc == 1) {
            leaf_type(x)->free(x);
        }
    }
}
int leaf_write(leaf_object *x, port *p) {
    if (x) {
        leaf_class *t = leaf_type(x);
        if (t->write) return t->write(x, p);
        else return port_printf(p, "#<leaf:%p>");
    }
    // for tuples that contain null pointers
    else {
        return port_printf(p, "#<null>");
    }
}
