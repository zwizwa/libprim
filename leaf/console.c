#include <leaf/console.h>
#include <stdlib.h>
#include <string.h>

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

leaf_object *console_read(console *d) {
    return parser_read(d->p);
}
console *console_new(port *in, port *out) {
    console *x = calloc(1, sizeof(*x));
    leaf_init(&x->base, console_type());
    x->in = in;
    x->out = out;
    x->p = parser_new(in);
    return x;
}

leaf_object *console_rpc(console *d, const char *cmd) {
    port_write(d->out, (void*)cmd, strlen(cmd));
    port_write(d->out, "\n", 1);
    port_flush(d->out);
    leaf_object *o = console_read(d);
    if (0) {
        leaf_write(o, NULL);
        fprintf(stderr, "\n");
    }
    return o;
}


bytes *console_rpc_bytes(console *d, const char *cmd) {
    leaf_object *rv = console_rpc(d, cmd);
    bytes *b = leaf_to_string(rv);
    leaf_free(rv);
    return b;
}
