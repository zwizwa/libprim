#include <stdlib.h>
#include <string.h>
#include "port.h"

/* FILE ports */
int port_file_vprintf(port *p, const char *fmt, va_list ap) {
    va_list aq;
    int len;
    /* We might be called from error reporting with broken state.
       Allow for a default. */
    FILE *f = p ? p->stream.file : stderr;
    va_copy(aq, ap);
    len = vfprintf(f, fmt, aq);
    va_end(aq);
    return len;
}
int port_file_getc(port *p) {
    return fgetc(p->stream.file);
}
int port_file_putc(port *p, int c) {
    return fputc(c, p->stream.file);
}
int port_file_write(port *p, void *buf, size_t len) {
    return fwrite(buf, 1, len, p->stream.file);
}
void port_file_close(port *x) {
    fclose(x->stream.file);
    x->stream.file = NULL;
}
void port_file_init(port *p) {
    p->vprintf = port_file_vprintf;
    p->get     = port_file_getc;
    p->put     = port_file_putc;
    p->write   = port_file_write;
    p->close   = port_file_close;
}

/* Bytes ports */


/* Virtual */
int port_printf(port *p, const char *fmt, ...) {
    int len;
    va_list ap; va_start (ap, fmt);
    len = p->vprintf(p, fmt, ap);
    va_end(ap);
    return len;
}
int port_vprintf(port *p, const char *fmt, va_list ap) {
    return p->vprintf(p, fmt, ap);
}
int port_getc(port *p) {
    return p->get(p);
}
int port_putc(port *p, int c) {
    return p->put(p, c);
}
int port_write(port *p, void *buf, size_t len) {
    return p->write(p, buf, len);
}
void port_close(port *p) {
    p->close(p);
}



void port_free(port *x) {
    /* fprintf(stderr, "port_free(%p)\n", x); */
    port_close(x);
    if (x->name) free (x->name);
    free(x);
}




port_class *port_class_new(void) {
    port_class *x = calloc(1, sizeof(*x));
    x->super.free = (leaf_free)port_free;
    return x;
}
port *port_new(port_class *type, FILE *f, const char *name) {
    if (!f) return NULL;
    port *x = malloc(sizeof(*x));
    port_file_init(x);
    x->type = type;
    x->stream.file = f;
    x->name = NULL;
    if (name) {
        x->name = malloc(1 + strlen(name));
        strcpy(x->name, name);
    }
    return x;
}

