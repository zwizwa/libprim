
#include <leaf/port.h>
#include <leaf/bytes.h>

#include <stdlib.h>

#define TOK_STRING      'B'
#define TOK_QUOTE       'Q'
#define TOK_QUASI_QUOTE 'A'
#define TOK_UNQUOTE     'U'
#define TOK_NUMER       'N'
#define TOK_SYMBOL      'S'
#define TOK_DOT         'D'
#define TOK_LEFT        'L'
#define TOK_RIGHT       'R'

/* Scheme-style s-expression tokenizer. */
typedef struct _scanner scanner;
typedef void (*eof_m)(scanner *x);
struct _scanner {
    port *p;
    bytes *b;
    eof_m cont_eof;
};

typedef bytes token;


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



token *make_token(scanner *x, char tag) {
    bytes *b = x->b; x->b = NULL;
    b->bytes[0] = tag;
    printf("%s\n", b->bytes);
    return b;
}

int char_in(int c, const char *str) {
    for(;;){
        if (!str[0]) return 0;
        if (c == str[0]) return 1;
        str++;
    }
}

int scanner_isterm(scanner *x, int c) {
    if (char_in(c, "()\',`#;\"") || isspace(c)) {
        port_ungetc(x->p, c); return 1;
    }
    return 0;
}

token *scanner_save(scanner *x, char c) {
    *bytes_allot(x->b, 1) = c;
}
void scanner_reset(scanner *x) {
    x->b->size = 1;
    x->b->bytes[0] = '?';
    x->b->bytes[1] = 0;
}

token *scanner_get_atom(scanner *x, char tag) {
    int c;
    for(;;) {
        c = scanner_getc(x);
        if (scanner_isterm(x, c)) return make_token(x, tag);
        scanner_save(x, c);
    }
}
token *make_0token(scanner *x, char tag) {
    scanner_reset(x);
    return make_token(x, tag);
}

token *scanner_get_hash(scanner *x) {
    int c = scanner_getc(x);
    scanner_reset(x);
    switch(c) {
    case '\\':
        return scanner_get_atom(x, 'C');
    }
}
token *scanner_get_string(scanner *x) {
    int c;
    scanner_reset(x);
    for(;;) {
        c = scanner_getc(x);
        if (c == '"') return make_token(x, 'B');
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
    if (!x->b) x->b = bytes_new(NULL, 20);
    scanner_reset(x);
    scanner_save(x, c = scanner_skip_getc(x));

    switch(c) {
    case '\'': return make_0token(x, 'Q');
    case '`':  return make_0token(x, 'A');
    case ',':  return make_0token(x, 'U'); // FIXME: unquote-spicing
    case '(':  return make_0token(x, 'L');
    case ')':  return make_0token(x, 'R');
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '.': 
        scanner_save(x, c = scanner_getc(x));
        if (isspace(c)) return make_0token(x, 'D');
        else {
            if (isdigit(c)) return scanner_get_atom(x, 'N');
            else return scanner_get_atom(x, 'S');
        }
    default:
        if (isdigit(c)) return scanner_get_atom(x, 'N');
        else return scanner_get_atom(x, 'S');
    }
}

void scanner_eof(scanner *x) {
    printf("EOF\n");
    exit(1);
}

int main(void) {
    scanner x;
    x.p = port_file_new(NULL, stdin, "<stdin>");
    x.b = NULL;
    x.cont_eof;
    for(;;) {
        scanner_get_token(&x);
    }
}
