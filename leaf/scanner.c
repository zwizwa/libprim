
/* Scheme-style s-expression scanner.  This produces bytes objects
   where the first character represents the token tag. */

#include <leaf/scanner.h>

#include <stdlib.h>
#include <ctype.h>




static int next_getc(scanner *x) {
    int c = port_getc(x->p);

    if ('\n' == c) {x->line++; x->col=1;}
    else {x->col++;}

    if (EOF == c) {
        x->cont_eof(x);  // continuation. does not return.
        printf("EOF\n");
        exit(1);
    }
    return c;
}

/* Skip whitespace before first character of token. */
static int first_getc(scanner *x) {
    int c;
  next:
    c = next_getc(x);
    if (isspace(c)) goto next;
    if (';' == c) {
        for (;;) {
            c = next_getc(x);
            if ('\n' == c) goto next;
        }
    }
    return c;
}



static void set_token(scanner *x, char tag) {
    x->b->bytes[0] = tag;
    // printf("%s\n", x->b->bytes);
}

static int char_in(int c, const char *str) {
    for(;;){
        if (!str[0]) return 0;
        if (c == str[0]) return 1;
        str++;
    }
}

static void save(scanner *x, char c) {
    *bytes_allot(x->b, 1) = c;
}

static void reset(scanner *x) {
    x->b->size = 1;
    x->b->bytes[0] = TOK_ERROR;
    x->b->bytes[1] = 0;
}

static void scanner_get_atom(scanner *x, char tag) {
    int c;
    for(;;) {
        c = next_getc(x);
        if (char_in(c, "()\',`#;\"") || isspace(c)) {
            port_ungetc(x->p, c);
            set_token(x, tag);
            return;
        }
        save(x, c);
    }
}
static void make_0token(scanner *x, char tag) {
    reset(x);
    set_token(x, tag);
}

static void scanner_get_hash(scanner *x) {
    int c = next_getc(x);
    reset(x);
    switch(c) {
    case '\\': scanner_get_atom(x, TOK_CHAR); break;
    default:   scanner_get_atom(x, TOK_HASH); break;
    }
}

static void scanner_get_string(scanner *x) {
    int c;
    reset(x);
    for(;;) {
        c = next_getc(x);
        if (c == '"') { 
            set_token(x, TOK_STRING); 
            return; 
        }
        if (c == '\\') {
            c = next_getc(x);
            switch(c) {
            case 'n': c = '\n'; break;
            case 't': c = '\n'; break;
            default: break;
            }
        }
        save(x, c);
    }
}

/* Public */
const bytes *scanner_token(scanner *x) {
    return x->b;
}
void scanner_read(scanner *x) {
    int c;
    if (!x->b) x->b = bytes_new(20);
    reset(x);
    save(x, c = first_getc(x));

    switch(c) {
    case '\'': return make_0token(x, TOK_QUOTE);
    case '`':  return make_0token(x, TOK_QUASI_QUOTE);
    case ',':  return make_0token(x, TOK_UNQUOTE); // FIXME: unquote-spicing
    case '(':  return make_0token(x, TOK_LEFT);
    case ')':  return make_0token(x, TOK_RIGHT);
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '.': 
        save(x, c = next_getc(x));
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
void scanner_free(scanner *x) {
    leaf_free((leaf_object*)x->b);
    free(x);
    // DON'T FREE PORT.
}
scanner *scanner_new(port *p) {
    scanner *x = calloc(1, sizeof(*x));
    x->p = p;
    x->b = bytes_new(100);
    x->cont_eof = scanner_eof;
    return x;
}

#if _SCANNER_TEST_
int main(void) {
    scanner *x = scanner_new(port_file_new(stdin, "<stdin>"));
    for(;;) {
        scanner_get_token(x);
    }
}
#endif
