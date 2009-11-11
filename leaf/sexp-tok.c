
/* Scheme-style s-expression tokenizer.  This produces bytes objects
   where the first character represents the token tag. */

#define TOK_STRING      '"'
#define TOK_NUMBER      '0'
#define TOK_SYMBOL      ':'
#define TOK_CHAR        '$'
#define TOK_HASH        '#'

#define TOK_QUASI_QUOTE      '`'
#define TOK_QUOTE            '\''
#define TOK_UNQUOTE          ','
#define TOK_UNQUOTE_SPLICING '@'

#define TOK_LEFT        '('
#define TOK_DOT         '.'
#define TOK_RIGHT       ')'

#define TOK_EOF         'E'
#define TOK_ERROR       '?'


#include <leaf/port.h>
#include <leaf/bytes.h>

#include <stdlib.h>
#include <ctype.h>


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

void scanner_save(scanner *x, char c) {
    *bytes_allot(x->b, 1) = c;
}
void scanner_reset(scanner *x) {
    x->b->size = 1;
    x->b->bytes[0] = TOK_ERROR;
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
        return scanner_get_atom(x, TOK_CHAR);
    default:
        return scanner_get_atom(x, TOK_HASH);
    }
    
}
token *scanner_get_string(scanner *x) {
    int c;
    scanner_reset(x);
    for(;;) {
        c = scanner_getc(x);
        if (c == '"') return make_token(x, TOK_STRING);
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
    if (!x->b) x->b = bytes_new(20);
    scanner_reset(x);
    scanner_save(x, c = scanner_skip_getc(x));

    switch(c) {
    case '\'': return make_0token(x, TOK_QUOTE);
    case '`':  return make_0token(x, TOK_QUASI_QUOTE);
    case ',':  return make_0token(x, TOK_UNQUOTE); // FIXME: unquote-spicing
    case '(':  return make_0token(x, TOK_LEFT);
    case ')':  return make_0token(x, TOK_RIGHT);
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '.': 
        scanner_save(x, c = scanner_getc(x));
        if (isspace(c)) return make_0token(x, TOK_DOT);
        break;
    default:
        break;
    }
    if (isdigit(c)) return scanner_get_atom(x, TOK_NUMBER);
    else return scanner_get_atom(x, TOK_SYMBOL);
}

void scanner_eof(scanner *x) {
    printf("EOF\n");
    exit(1);
}

#if _TEST_
int main(void) {
    scanner x;
    x.p = port_file_new(stdin, "<stdin>");
    x.b = NULL;
    x.cont_eof;
    for(;;) {
        scanner_get_token(&x);
    }
}
#endif
