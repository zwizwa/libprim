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
static void _write_hex(FILE *f, int val, int digits) {
    while (digits-- > 0) {
        fputc(hexdigit[(val >> (digits * 4)) & 0xF], f);
    }
}

/* This prints full-length byte buffers, not C strings. */
void bytes_write_string(bytes *b, FILE *f) {
    size_t i = 0;
    int c;

    fputc('"',f);
    for(i = 0; i<b->size; i++) {
        c = b->bytes[i];
        if (c == '"')  { fputc('\\',f); fputc('"',f); }
        else if (c == '\n') { fputc('\\', f); fputc('n',f); }
        else if (c == '\\') { fputc('\\', f); fputc('\\',f); }
        else if ((c >= 32) && (c < 127)) fputc(c, f);
        else {fputc('\\',f); fputc('x',f); _write_hex(f, c, 2);}
    }
    fputc('"',f);
}

void bytes_dump(bytes *b, FILE *f) {
    int i,j;
    for(j = 0; j < b->size; j += 16) {
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { fputc(' ', f); fputc(' ', f); }
            else _write_hex(f, b->bytes[i+j], 2);
            fputc(' ', f);
        }
        fputc(' ', f);
        for (i = 0; i < 16; i++) {
            if ((i + j) >= b->size) { fputc(' ', f); }
            else {
                int c = b->bytes[i+j];
                if ((c >= 32) && (c < 127)) fputc(c, f);
                else fputc('.', f);
            }
        }
        fputc('\n', f);
    }
}
