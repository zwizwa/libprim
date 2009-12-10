#include <leaf/leaf.h>
#include <leaf/port.h>

void leaf_free(leaf_object *x) {
    if (x) {
        int rc = leaf_rc(x);
        if (rc == 1) {
            leaf_type(x)->_free(x);
        }
        else if (rc > 1) {
            leaf_rc_dec(x);
        }
        else {
            leaf_write(x, NULL);
            port_printf(NULL, ": invalid RC count %d\n", rc);
        }
    }
}
int leaf_write(leaf_object *x, port *p) {
    if (x) {
        leaf_class *t = leaf_type(x);
        if (t && t->_write) return t->_write(x, p);
        else return port_printf(p, "#<leaf:%p>");
    }
    // for tuples that contain null pointers
    else {
        return port_printf(p, "#<null>");
    }
}
