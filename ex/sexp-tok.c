
#include <leaf/port.h>
#include <leaf/bytes.h>

#include <stdlib.h>

/* Scheme-style s-expression tokenizer. */
typedef struct _scanner scanner;
typedef void (*eof_m)(scanner *x);
struct _scanner {
    port *p;
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
    if (char_in(c, "()\',`") || isspace(c)) {
        port_ungetc(x->p, c); return 1;
    }
    return 0;
}

token *scanner_get_atom(scanner *x, bytes *b, const char *tag) {
    int c;
    for(;;) {
        c = scanner_getc(x);
        if (scanner_isterm(x, c)) return make_token(tag, b);
        *bytes_allot(b, 1) = c;
    }
}
token *make_0token(const char *name) { 
    return make_token(name, NULL); 
}

token *scanner_get_token(scanner *x) {
    int c = scanner_skip_getc(x);
    bytes *b =  NULL;
    char head[] = {0,0,0};

    switch(c) {
    case '\'': return make_0token("quote");
    case '`':  return make_0token("quasi-unquote");
    case ',':  return make_0token("unquote"); // FIXME: unquote-spicing
    case '(':  return make_0token("LP");
    case ')':  return make_0token("RP");
        // case '#':  return scanner_get_hash(x);
        // case '"':  return scanner_get_string(x);
    case '.': 
        head[0] = '.';
        head[1] = c = scanner_getc(x);
        if (isspace(c)) return make_token("DOT", NULL);
        else {
            b = bytes_from_cstring(NULL, head);
            if (isdigit(c)) return scanner_get_atom(x, b, "number");
            else return scanner_get_atom(x, b, "symbol");
        }
    default:
        head[0] = c;
        b = bytes_from_cstring(NULL, head);
        if (isdigit(c)) return scanner_get_atom(x, b, "number");
        else return scanner_get_atom(x, b, "symbol");
    }
}

void scanner_eof(scanner *x) {
    printf("EOF\n");
    exit(1);
}

int main(void) {
    scanner x;
    x.p = port_file_new(NULL, stdin, "<stdin>");
    x.cont_eof;
    for(;;) {
        scanner_get_token(&x);
    }
}
