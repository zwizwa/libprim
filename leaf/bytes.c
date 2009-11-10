#include <stdlib.h>
#include <string.h>
#include "bytes.h"

static void _bytes_free(bytes *x) {
    free(x->bytes);
    free(x);
}
bytes_class *bytes_class_new(void) {
    bytes_class *x = malloc(sizeof(*x));
    x->super.free = (leaf_free)_bytes_free;
    return x;
}
bytes *bytes_new(bytes_class *type, size_t size) {
    bytes *x = malloc(sizeof(*x));
    x->type = type;
    x->size    = size;
    x->bufsize = size;
    x->bytes = malloc(size);
    return x;
}



char *cstring_from_bytes(bytes *b) {
    if ((b->size + 1) < b->bufsize) return NULL;
    if (b->bytes[b->size] != 0) return NULL;
    return b->bytes;
}

bytes* bytes_from_cstring(bytes_class *type, const char *str){
    size_t len = strlen(str);
    bytes *b = bytes_new(type, 1+len);
    strcpy(b->bytes, str);
    b->size = len;
    return b;
}

// dequote backslashes
static inline int fromhexdigit(int c, char digit) {
    int d = 0;
    if ((digit >= '0') && (digit <= '9')) d = digit - '0';
    else if ((digit >= 'A') && (digit <= 'F')) d = 10 + digit - 'A';
    else if ((digit >= 'a') && (digit <= 'f')) d = 10 + digit - 'a';
    return (c << 4) + (d & 15);
}
bytes* bytes_from_qcstring(bytes_class *type, const char *str){
    bytes *b = bytes_from_cstring(type, str);
    int dst, src;
    for (src=0,dst=0; src < b->size; src++,dst++) {
        int c;
        if ((c = b->bytes[src]) == '\\') {
            c = b->bytes[++src];
            if ('x' == c) { 
                c = 0;
                c = fromhexdigit(c, b->bytes[++src]);
                c = fromhexdigit(c, b->bytes[++src]);
            }
            else if ('n' == c) { c = '\n'; }
            else if ('r' == c) { c = '\r'; }
            else if ('t' == c) { c = '\t'; }
        }
        b->bytes[dst] = c;
    }
    b->size = dst;
    b->bytes[dst] = 0;
    return b;
}

static const char hexdigit[] = "0123456789ABCDEF";
static void _write_hex(port *p, int val, int digits) {
    while (digits-- > 0) {
        port_putc(p, hexdigit[(val >> (digits * 4)) & 0xF]);
    }
}

/* This prints full-length byte buffers, not C strings. */
void bytes_write_string(bytes *b, port *p) {
    size_t i = 0;
    int c;

    port_putc(p, '"');
    for(i = 0; i<b->size; i++) {
        c = b->bytes[i];
        if (c == '"')  { port_putc(p, '\\'); port_putc(p, '"'); }
        else if (c == '\n') { port_putc(p, '\\'); port_putc(p, 'n'); }
        else if (c == '\\') { port_putc(p, '\\'); port_putc(p, '\\'); }
        else if ((c >= 32) && (c < 127)) port_putc(p, c);
        else {port_putc(p, '\\'); port_putc(p, 'x'); _write_hex(p, c, 2);}
    }
    port_putc(p, '"');
}

void bytes_dump(bytes *b, port *p) {
    int i,j;
    for(j = 0; j < b->size; j += 16) {
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { port_putc(p, ' '); port_putc(p, ' '); }
            else _write_hex(p, b->bytes[i+j], 2);
            port_putc(p, ' ');
        }
        port_putc(p, ' ');
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { port_putc(p, ' '); }
            else {
                int c = b->bytes[i+j];
                if ((c >= 32) && (c < 127)) port_putc(p, c);
                else port_putc(p, '.');
            }
        }
        port_putc(p, '\n');
    }
}


void bytes_realloc(bytes *b, size_t size) {
    if (!(b->bytes = realloc(b->bytes, size))) {
        b->size = b->bufsize = 0;
    }
}
void *bytes_allot(bytes *b, size_t extra) {
    int len = extra + b->size;
    if ((1 + len) > b->bufsize) {
        bytes_realloc(b, (1 + len));
    }
    void *start = b->bytes + b->size;
    b->size += extra;
    b->bytes[b->size] = 0; // zero-terminate
    return start;
}

bytes *bytes_copy(bytes* b) {
    bytes *new_b = bytes_new(b->type, b->bufsize);
    memcpy(new_b->bytes, b->bytes, b->bufsize);
    return new_b;
}
