#include <stdlib.h>
#include <string.h>
#include "port.h"
#include <leaf/bytes.h>

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
int port_file_ungetc(port *p, int c) {
    return ungetc(c, p->stream.file);
}
int port_file_write(port *p, void *buf, size_t len) {
    return fwrite(buf, 1, len, p->stream.file);
}
void port_file_close(port *x) {
    fclose(x->stream.file);
    x->stream.file = NULL;
}
bytes *port_file_bytes(port *x) { return NULL; }
void port_file_init(port *p) {
    p->vprintf = port_file_vprintf;
    p->get     = port_file_getc;
    p->unget   = port_file_ungetc;
    p->put     = port_file_putc;
    p->write   = port_file_write;
    p->close   = port_file_close;
    p->bytes   = port_file_bytes;
}

/* Bytes ports */
int port_bytes_vprintf(port *p, const char *fmt, va_list ap) {
    if (!p->stream.b.bytes) return -1;
    va_list aq;
    int len;
    va_copy(aq, ap);
    len = vsnprintf(NULL, 0, fmt, aq);
    va_end(aq);
    if (len < 0) return len;
    void *data = bytes_allot(p->stream.b.bytes, len);
    len = vsprintf(data, fmt, ap);
    return len;
}
int port_bytes_getc(port *p) {
    if (!p->stream.b.bytes) return -1;
    int i;
    if ((i = p->stream.b.read_index) >= p->stream.b.bytes->size) return EOF;
    int c = p->stream.b.bytes->bytes[i];
    p->stream.b.read_index++;
    return c;
}
int port_bytes_ungetc(port *p, int c) {
    if (!p->stream.b.bytes) return -1;
    if (p->stream.b.bytes->size > 0) {
        p->stream.b.bytes->bytes[--p->stream.b.bytes->size] = c;
        return c;
    }
    else return EOF;
}
int port_bytes_putc(port *p, int c) {
    if (!p->stream.b.bytes) return -1;
    char *data = bytes_allot(p->stream.b.bytes, 1);
    data[0] = c;
    return c;
}
int port_bytes_write(port *p, void *buf, size_t len) {
    if (!p->stream.b.bytes) return -1;
    void *data = bytes_allot(p->stream.b.bytes, len);
    memcpy(data, buf, len);
    return len;
}
void port_bytes_close(port *p) {
    if (p->stream.b.bytes) leaf_free((leaf_object*)(p->stream.b.bytes));
    p->stream.b.bytes = NULL;
}
bytes *port_bytes_bytes(port *p) {
    bytes *b = p->stream.b.bytes; 
    p->stream.b.bytes = NULL;
    return b;
}
void port_bytes_init(port *p) {
    p->vprintf = port_bytes_vprintf;
    p->get     = port_bytes_getc;
    p->unget   = port_bytes_ungetc;
    p->put     = port_bytes_putc;
    p->write   = port_bytes_write;
    p->close   = port_bytes_close;
    p->bytes   = port_bytes_bytes;
}



/* Virtual */
int port_printf(port *p, const char *fmt, ...) {
    int len;
    va_list ap;
    va_start (ap, fmt);
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
int port_ungetc(port *p, int c) {
    return p->unget(p, c);
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
bytes *port_get_bytes(port *p) {
    return p->bytes(p);
}


void port_free(port *x) {
    /* fprintf(stderr, "port_free(%p)\n", x); */
    port_close(x);
    if (x->name) free (x->name);
    free(x);
}



static port_class *type = NULL;
static port_class *port_class_new(void) {
    port_class *x = calloc(1, sizeof(*x));
    x->super.free = (leaf_free_m)port_free;
    return x;
}
port_class *port_type(void) {
    if (!type) type = port_class_new();
    return type;
}
port *port_file_new(FILE *f, const char *name) {
    if (!f) return NULL;
    port *x = calloc(1, sizeof(*x));
    port_file_init(x);
    x->type = port_type();
    x->stream.file = f;
    x->name = NULL;
    if (name) {
        x->name = malloc(1 + strlen(name));
        strcpy(x->name, name);
    }
    return x;
}
port *port_bytes_new(bytes *b) {
    if (!b) return NULL;
    port *x = calloc(1, sizeof(*x));
    port_bytes_init(x);
    x->type = port_type();
    x->stream.b.bytes = b;
    x->stream.b.read_index = 0;
    x->name = malloc(9);
    strcpy(x->name, "<string>");
    return x;
}
