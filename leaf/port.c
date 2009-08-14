#include <stdlib.h>
#include <string.h>
#include "port.h"
int port_vprintf(port *p, char *fmt, va_list ap) {
    va_list aq;
    int len;
    va_copy(aq, ap);
    len = vfprintf(p->stream, fmt, aq);
    va_end(aq);
    return len;
}
int port_printf(port *p, char *fmt, ...) {
    int len;
    va_list ap; va_start (ap, fmt);
    len = port_vprintf(p, fmt, ap);
    va_end(ap);
    return len;
}
static void _port_free(port *x) {
    fclose(x->stream);
    if (x->name) free (x->name);
    free(x);
}
port_class *port_class_new(void) {
    port_class *x = malloc(sizeof(*x));
    x->free = _port_free;
    return x;
}
port *port_new(port_class *type, FILE *f, const char *name) {
    port *x = malloc(sizeof(*x));
    x->type = type;
    x->stream = f;
    x->name = NULL;
    if (name) {
        x->name = malloc(1 + strlen(name));
        strcpy(x->name, name);
    }
    return x;
}