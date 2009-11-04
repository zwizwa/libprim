#include <stdlib.h>
#include <string.h>
#include "bytes.h"

static void _bytes_free(bytes *x) {
    free(x->bytes);
    free(x);
}
bytes_class *bytes_class_new(void) {
    bytes_class *x = malloc(sizeof(*x));
    x->free = _bytes_free;
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

static const char hexdigit[] = "0123456789ABCDEF";
static void _write_hex(FILE *f, int val, int digits) {
    while (digits >= 0) {
        fputc(hexdigit[val & 0xF], f);
        val = (val >> 4);
        digits--;
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
