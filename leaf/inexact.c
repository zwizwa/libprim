#include <leaf/inexact.h>
#include <stdlib.h>
#include <leaf/port.h>


static void inexact_free(inexact *x) { free (x); }

static int inexact_write(inexact *x, port *p) {
    return port_printf(p, "%lf", x->value);
}

LEAF_SIMPLE_TYPE(inexact)


inexact *inexact_new(double f) {
    inexact *x = malloc(sizeof(*x));
    x->type = inexact_type();
    x->value = f;
    return x;
}
