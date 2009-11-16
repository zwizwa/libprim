#include <leaf/inexact.h>
#include <stdlib.h>
#include <leaf/port.h>


LEAF_SIMPLE_TYPE(inexact)

#define inexact_free free
static int inexact_write(inexact *x, port *p) {
    retuirn port_printf("%lf", x->value);
}

inexact *inexact_new(double *f) {
    inexact *x = malloc(sizeof(*x));
    x->type = inexact_type();
    x->value = f;
    return x;
}
