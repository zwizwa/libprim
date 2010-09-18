/* (c) 2002-2009 Tom Schouten

   Part of this file is adapted from PacketForth (c) Tom Schouten,
   which is licenced under the GPL.  However, this file is part of
   core libprim and licenced under the LGPL. */


#include "config.h"
#include "port.h"

#include <stdlib.h>
#include <string.h>

/* FILE ports */
int port_file_vprintf(port *p, const char *fmt, va_list ap) {
    va_list aq;
    int len;
    /* We might be called from error reporting with broken state.
       Allow for a default. */
    FILE *f = p ? p->stream.f.file : stderr;
    va_copy(aq, ap);
    len = vfprintf(f, fmt, aq);
    va_end(aq);
    return len;
}
int port_file_getc(port *p) {
    return fgetc(p->stream.f.file);
}
int port_file_putc(port *p, int c) {
    return fputc(c, p->stream.f.file);
}
int port_file_ungetc(port *p, int c) {
    return ungetc(c, p->stream.f.file);
}
int port_file_write(port *p, void *buf, size_t len) {
    int rv = fwrite(buf, 1, len, p->stream.f.file);
#ifdef PORT_DEBUG
    // fprintf(stderr, "WRITTEN %d\n", (int)len);
    if (rv != len) {
        int rv;
        if ((rv = feof(p->stream.f.file))) fprintf(stderr, "WRITE EOF: %d\n", rv);
        else if ((rv = ferror(p->stream.f.file))) fprintf(stderr, "WRITE ERROR: %d\n", rv);
    }
#endif
    return rv;

}
int port_file_read(port *p, void *buf, size_t len) {
    int rv = fread(buf, 1, len, p->stream.f.file);

    // it might actually be simpler to forget about ERROR/EOF..
#ifdef PORT_DEBUG
    if (rv < len) {
        int rv;
        if ((rv = feof(p->stream.f.file))) fprintf(stderr, "READ EOF: %d\n", rv);
        else if ((rv = ferror(p->stream.f.file))) fprintf(stderr, "READ ERROR: %d\n", rv);
        // return -1;
    }
#endif
    return rv;
}

void port_file_close(port *x) {
    fclose(x->stream.f.file);
    x->stream.f.file = NULL;
}
void port_file_flush(port *x) {
    fflush(x->stream.f.file);
}
bytes *port_file_bytes(port *x) { return NULL; }

void port_methods_file_init(port_methods *p) {
    p->vprintf = port_file_vprintf;
    p->get     = port_file_getc;
    p->unget   = port_file_ungetc;
    p->put     = port_file_putc;
    p->write   = port_file_write;
    p->read    = port_file_read;
    p->close   = port_file_close;
    p->bytes   = port_file_bytes;
    p->flush   = port_file_flush;
}
/* Bytes ports */

int port_bytes_vprintf(port *p, const char *fmt, va_list ap) {
    if (!p->stream.b.bytes) return -1;
    return bytes_vprintf(p->stream.b.bytes, fmt, ap);
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
    if (p->stream.b.read_index > 0) {
        p->stream.b.bytes->bytes[--(p->stream.b.read_index)] = c;
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
void port_bytes_flush(port *p) {}

int port_bytes_read(port *p, void *b, size_t len) {
    exit(1);
}

void port_methods_bytes_init(port_methods *p) {
    p->vprintf = port_bytes_vprintf;
    p->get     = port_bytes_getc;
    p->unget   = port_bytes_ungetc;
    p->put     = port_bytes_putc;
    p->write   = port_bytes_write;
    p->read    = port_bytes_read;
    p->close   = port_bytes_close;
    p->bytes   = port_bytes_bytes;
    p->flush   = port_bytes_flush;
}

/* Default */
static port *_default_out = NULL;
static port *default_out(void) {
    if (!_default_out) _default_out = port_file_new(stderr, "<stderr>");
    return _default_out;
}


/* Virtual */
int port_printf(port *p, const char *fmt, ...) {
    if (!(p && p->m->vprintf)) p = default_out();
    int len;
    va_list ap;
    va_start (ap, fmt);
    len = p->m->vprintf(p, fmt, ap);
    va_end(ap);
    return len;
}
int port_vprintf(port *p, const char *fmt, va_list ap) {
    if (!(p && p->m->vprintf)) p = default_out();
    return p->m->vprintf(p, fmt, ap);
}
int port_getc(port *p) {
    if (!(p && p->m->get)) return EOF;
    return p->m->get(p);
}
int port_ungetc(port *p, int c) {
    if (!(p && p->m->unget)) return EOF;
    return p->m->unget(p, c);
}
int port_putc(port *p, int c) {
    if (!(p && p->m->put)) p = default_out();
    return p->m->put(p, c);
}
int port_write(port *p, void *buf, size_t len) {
    if (!(p && p->m->write)) p = default_out();
    return p->m->write(p, buf, len);
}
int port_read(port *p, void *buf, size_t len) {
    if (!(p && p->m->read)) return EOF;
    return p->m->read(p, buf, len);
}
void port_close(port *p) {
    if (p && p->m->close) p->m->close(p);
}
void port_flush(port *p) {
    if (p && p->m->flush) p->m->flush(p);
}
bytes *port_get_bytes(port *p) {
    if (!(p && p->m->bytes)) return NULL;
    return p->m->bytes(p);
}


void port_free(port *x) {
    /* fprintf(stderr, "port_free(%p)\n", x); */
    port_close(x);
    if (x->name) free (x->name);
    free(x);
}


static int port_write_info(port *x, port *p) {
    return port_printf(p, "#<port:%s>", x->name);
}

static port_class *type = NULL;
static port_methods *methods_file = NULL;
static port_methods *methods_bytes = NULL;

leaf_class *port_type(void) {
    if (!type) {
        type = calloc(1, sizeof(*type));
        leaf_class_init((leaf_class*)type, (leaf_free_m)port_free, (leaf_write_m)port_write_info);

        methods_file = calloc(1, sizeof(*methods_file));
        port_methods_file_init(methods_file);

        methods_bytes = calloc(1, sizeof(*methods_bytes));
        port_methods_bytes_init(methods_bytes);
    }
    return (leaf_class*)type;
}
port *port_file_new(FILE *f, const char *name) {
    if (!f) return NULL;
    port *x = calloc(1, sizeof(*x));
    leaf_init(&x->base, port_type());
    x->m = methods_file;
    x->stream.f.file = f;
    x->stream.f.fd = fileno(f);
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
    leaf_init(&x->base, port_type());
    x->m = methods_bytes;
    x->stream.b.bytes = b;
    x->stream.b.read_index = 0;
    x->name = malloc(9);
    strcpy(x->name, "<string>");
    return x;
}


bytes *port_slurp(port *p) {
    int bs = 4096, total = 0, chunk = 0;
    bytes *b = bytes_buffer_new(bs);
    do {
        // fprintf(stderr, "slurp, total %d\n", total);

        void *buf = bytes_allot(b, bs);
        chunk = port_read(p, buf, bs);
        total += chunk;
    } while(chunk);
    b->size = total;
    return b;
}

int port_fd(port *p) {
    if (p->m == methods_file) {
        return p->stream.f.fd;
    }
    return -1;
}

bytes *leaf_to_string(leaf_object *o) {
    port *p = port_bytes_new(bytes_buffer_new(100));
    leaf_write(o, p);
    bytes *b = port_get_bytes(p);
    leaf_free((leaf_object*)p);
    return b;    
}
