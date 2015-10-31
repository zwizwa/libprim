#include <stdlib.h>
#include <string.h>
#include "bytes.h"

static void _bytes_free(bytes *x) {
    if (x->bufsize) { // don't free const
        free(x->bytes);
    }
    free(x);
}

int bytes_dump(bytes *x, port *p) {
    return port_write(p, x->bytes, x->size);
}
static bytes_class *type = NULL;
static bytes_class *bytes_class_new(void) {
    bytes_class *x = calloc(1, sizeof(*x));
    leaf_class_init((leaf_class*)x, "bytes", (leaf_free_m)_bytes_free, (leaf_write_m)bytes_write_string);
    x->super.dump  = (leaf_write_m)bytes_dump;
    return x;
}
leaf_class *bytes_type(void) {
    if (!type) type = bytes_class_new();
    return (leaf_class*)type;
}




static void bytes_init(bytes *x, void *buf, size_t size, size_t bufsize) {
    leaf_init(&x->base, bytes_type());
    x->size = size;
    x->bufsize = bufsize;
    x->bytes = buf;
}

// don't use this for strings!
bytes *bytes_new(size_t size, size_t bufsize) {
    bytes *b = malloc(sizeof(*b));
    bytes_init(b, malloc(bufsize), size, bufsize);
    return b;
}
bytes *bytes_buffer_new(size_t bufsize) {
    return bytes_new(0, bufsize);
}

/* Wrap a const string in a bytes object. */
bytes *bytes_const_new(const char *str, size_t size) {
    bytes *b = malloc(sizeof(*b));
    bytes_init(b, (char*)str, size, 0 /* const: bufsize == 0 */);
    return b;
}



char *cstring_from_bytes(const bytes *b) {
    /* Only works for null-terminated strings. */
    if (b->bytes[b->size] != 0) return NULL;
    return b->bytes;
}

bytes* bytes_from_cstring(const char *str){
    size_t len = strlen(str);
    bytes *b = bytes_new(len, 1+len);
    strcpy(b->bytes, str);
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
bytes* bytes_from_qcstring(const char *str){
    bytes *b = bytes_from_cstring(str);
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
static int _write_hex(port *p, int val, int digits) {
    while (digits-- > 0) {
        port_putc(p, hexdigit[(val >> (digits * 4)) & 0xF]);
    }
    return digits;
}

/* This prints full-length byte buffers, not C strings. */
int bytes_write_string(bytes *b, port *p) {
    size_t i = 0;
    int num = 0;
    int c;

    port_putc(p, '"'); num++;
    for(i = 0; i<b->size; i++) {
        c = b->bytes[i];
        if (c == '"')  { port_putc(p, '\\'); port_putc(p, '"'); num += 2; }
        else if (c == '\n') { port_putc(p, '\\'); port_putc(p, 'n'); num += 2;}
        else if (c == '\\') { port_putc(p, '\\'); port_putc(p, '\\'); num += 2;}
        else if ((c >= 32) && (c < 127)) {port_putc(p, c); num ++; }
        else {port_putc(p, '\\'); port_putc(p, 'x'); _write_hex(p, c, 2); num += 4;}
    }
    port_putc(p, '"'); num ++;
    return num;
}

int bytes_hexdump(bytes *b, port *p) {
    int i,j;
    int len = 0;
    for(j = 0; j < b->size; j += 16) {
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { port_putc(p, ' '); port_putc(p, ' '); len+=2; }
            else len += _write_hex(p, b->bytes[i+j], 2);
            port_putc(p, ' '); len++;
        }
        port_putc(p, ' '); len++;
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { port_putc(p, ' '); len++; }
            else {
                int c = b->bytes[i+j];
                if ((c >= 32) && (c < 127)) { port_putc(p, c); len++; }
                else { port_putc(p, '.'); len++; }
            }
        }
        port_putc(p, '\n'); len++;
    }
    return len;
}


void bytes_realloc(bytes *b, size_t size) {
    if (b->bufsize == 0) {
        /* If it's a const, copy to writable memory. */
        void *buf = calloc(1,size);
        memcpy(buf, b->bytes, b->size);
        b->bufsize = size;
        b->bytes = buf;
    }
    else if ((b->bytes = realloc(b->bytes, size))) {
        b->bufsize = size;
    }
    else {
        b->size = b->bufsize = 0;
    }
}
char *bytes_allot(bytes *b, size_t extra) {
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
    int nb_bytes = b->size + 1;
    bytes *new_b = bytes_new(b->size, nb_bytes);
    memcpy(new_b->bytes, b->bytes, nb_bytes);
    return new_b;
}

int bytes_vprintf(bytes *b, const char *fmt, va_list ap) {
    va_list aq;
    int len;
    va_copy(aq, ap);
    len = vsnprintf(NULL, 0, fmt, aq);
    va_end(aq);
    if (len < 0) return len;
    void *data = bytes_allot(b, len);
    len = vsprintf(data, fmt, ap);
    return len;
}

