#include <leaf/console.h>
#include <stdlib.h>

static void console_free(console *x) {
    leaf_free((leaf_object*)x->p);
    leaf_free((leaf_object*)x->in);  // parser doesn't own port!
    leaf_free((leaf_object*)x->out);
    free(x);
}
static int console_write(console *x, port *p) {
    return port_printf(p, "#<console>");
}
LEAF_SIMPLE_TYPE(console)

int console_write_raw(console *d, const char *buf, size_t size) {
    return port_write(d->out, (void*)buf, size);
}
leaf_object *console_read(console *d) {
    return parser_read(d->p);
}
console *console_new(port *in, port *out) {
    console *x = calloc(1, sizeof(*x));
    x->type = console_type();
    x->in = in;
    x->out = out;
    x->p = parser_new(in);
    return x;
}

