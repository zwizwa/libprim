
#include <leaf/port.h>
#include <leaf/bytes.h>

#include <stdlib.h>

/* Scheme-style s-expression tokenizer. */
typedef struct _scanner scanner;
typedef void (*eof_m)(scanner *x);
struct _scanner {
    port *p;
    bytes *b;
    eof_m cont_eof;
};

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
    printf("(%s %s)\n", tag, b ? b->bytes : NULL);
    return NULL;
}

int char_in(int c, const char *str) {
    for(;;){
        if (!str[0]) return 0;
        if (c == str[0]) return 1;
        str++;
    }
}

int scanner_isterm(scanner *x, int c) {
    if (char_in(c, "()\',`#") || isspace(c)) {
        port_ungetc(x->p, c); return 1;
    }
    return 0;
}

token *scanner_save(scanner *x, char c) {
    *bytes_allot(x->b, 1) = c;
}
void scanner_reset(scanner *x) {
    x->b->size = 0;
    x->b->bytes[0] = 0;
}

token *scanner_get_atom(scanner *x, const char *tag) {
    int c;
    for(;;) {
        c = scanner_getc(x);
        if (scanner_isterm(x, c)) return make_token(tag, x->b);
        scanner_save(x, c);
    }
}
token *make_0token(const char *name) { 
    return make_token(name, NULL); 
}

token *scanner_get_hash(scanner *x) {
    int c = scanner_getc(x);
    scanner_reset(x);
    switch(c) {
    case '\\':
        return scanner_get_atom(x, "char");
    }
}
token *scanner_get_string(scanner *x) {
    int c;
    scanner_reset(x);
    for(;;) {
        c = scanner_getc(x);
        if (c == '"') return make_token("string", x->b);
        if (c == '\\') {
            c = scanner_getc(x);
            switch(c) {
            case 'n': c = '\n'; break;
            case 't': c = '\n'; break;
            default: break;
            }
        }
        scanner_save(x, c);
    }
}

token *scanner_get_token(scanner *x) {
    int c;
    scanner_reset(x);
    scanner_save(x, c = scanner_skip_getc(x));

    switch(c) {
    case '\'': return make_0token("quote");
    case '`':  return make_0token("quasi-unquote");
    case ',':  return make_0token("unquote"); // FIXME: unquote-spicing
    case '(':  return make_0token("LP");
    case ')':  return make_0token("RP");
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '.': 
        scanner_save(x, c = scanner_getc(x));
        if (isspace(c)) return make_token("DOT", NULL);
        else {
            if (isdigit(c)) return scanner_get_atom(x, "number");
            else return scanner_get_atom(x, "symbol");
        }
    default:
        if (isdigit(c)) return scanner_get_atom(x, "number");
        else return scanner_get_atom(x, "symbol");
    }
}

void scanner_eof(scanner *x) {
    printf("EOF\n");
    exit(1);
}

int main(void) {
    scanner x;
    x.p = port_file_new(NULL, stdin, "<stdin>");
    x.b = bytes_new(NULL, 10); x.b->size = 0;
    x.cont_eof;
    for(;;) {
        scanner_get_token(&x);
    }
}
