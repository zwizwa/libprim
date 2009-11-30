#include <leaf/drone.h>
#include <stdlib.h>

static void drone_free(drone *x) {
    leaf_free((leaf_object*)x->p);
    leaf_free((leaf_object*)x->in);  // parser doesn't own port!
    leaf_free((leaf_object*)x->out);
    free(x);
}
static int drone_write(drone *x, port *p) {
    return port_printf(p, "#<drone>");
}
LEAF_SIMPLE_TYPE(drone)

int drone_write_raw(drone *d, void *buf, size_t size) {
    return port_write(d->out, buf, size);
}
leaf_object *drone_read(drone *d) {
    return parser_read(d->p);
}
drone *drone_new(port *in, port *out) {
    drone *x = calloc(1, sizeof(*x));
    x->type = drone_type();
    x->in = in;
    x->out = out;
    x->p = parser_new(in);
    return x;
}

