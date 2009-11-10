
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

int scanner_isterm(scanner *x, int c) {
    int rv = 0;
    switch(c) {
    case '(': 
    case ')':
        rv = 1; break;
    default:
        if (isspace(d)) rv = 1;
        break;
    }
    if (rv) port_ungetc(x->p, c);
    return rv;
}

token *scanner_get_atom(scanner *x, bytes *b, tag) {
    int c;
    for(;;) {
        c = scanner_getc(x);
        if (scanner_isterm(x, c)) return make_token(tag, b);
        *bytes_allot(b, 1) = c;
    }
}
token *scanner_get_token(scanner *x) {
    int c = scanner_skip_getc(x);
    bytes *b =  NULL;
    char head[] = {0,0,0};

    switch(c) {
    case '\'': return make_token("quote");
    case '`':  return make_token("quasi-unquote");
    case ',':  return make_token("unquote"); // FIXME: unquote-spicing
    case '(':  return make_token("LP", NULL);
    case ')':  return make_token("RP", NULL);
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '.': 
        head[0] = '.';
        head[1] = c = scanner_getc(x);
        if (isspace(c)) return make_token("DOT", NULL);
        else {
            b = bytes_from_cstring(head);
            if (isdigit(c)) return scanner_get_atom(x, b, "number");
            else return scanner_get_atoml(x, b, "symbol");
        }
    default:
        head[0] = c;
        b = bytes_from_cstring(head);
        if (isdigit(c)) return scanner_get_number(x, b);
        else return scanner_get_symbol(x, b);
    }
}
