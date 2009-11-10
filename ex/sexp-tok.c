
#include <leaf/port.h>
#include <leaf/bytes.h>

/* Scheme-style s-expression tokenizer. */
typedef void (*eof_m)(port *p);
typedef struct {
    port *p;
    eof_m cont_eof;
} scanner;

typedef void token;


int scanner_getc(scanner *x) {
    int c = port_getc(x->p);
    if (EOF == c) {
        x->cont_eof(x);  // continuation. does not return.
        printf("EOF\n");
        exit(1);
    }
    return c;
}

int scanner_skip_getc(scanner *x) {
    int c;
  next:
    c = scanner_getc(x);
    if (isspace(c)) goto next;
    if (';' == c) {
        for (;;) {
            c = scanner_getc(x);
            if ('\n' == c) goto next;
        }
    }
    return c;
}



token *make_token(const char *tag, bytes *b) {
    printf("(%s %s)\n", tag, b->bytes);
    return NULL;
}

token *scanner_get_symbol(scanner *x, bytes *b) {
    int c;
    for(;;) {
        c = scanner_getc(x);
        if (isspace(c)) return make_token("symbol", b);
        *bytes_allot(b, 1) = c;
    }
}

token *scanner_get_token(scanner *x) {
    int c = scanner_skip_getc(x);
    bytes *b =  NULL;
    char head[] = {0,0,0};

    switch(c) {
    case '(':  return make_token("LP", NULL);
    case ')':  return make_token("RP", NULL);
    case '#':  return tok_get_hash(x);
    case '"':  return tok_get_string(x);
    case '.': 
        head[0] = '.';
        head[1] = c = scanner_getc(x);
        if (isspace(c)) return make_token("DOT", NULL);
        else {
            b = bytes_from_cstring(head);
            if (isdigit(c)) return scanner_get_number(x, b);
            else return scanner_get_symbol(x, b);
        }
    default:
        head[0] = c;
        b = bytes_from_cstring(head);
        if (isdigit(c)) return scanner_get_number(x, b);
        else return scanner_get_symbol(x, b);
    }
}
