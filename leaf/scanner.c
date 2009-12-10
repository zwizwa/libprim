
/* Scheme-style s-expression scanner.  This produces bytes objects
   where the first character represents the token tag. */

#include <leaf/scanner.h>

#include <stdlib.h>
#include <ctype.h>
#include <string.h>

static int scanner_write(scanner *x, port *p) {
    return port_printf(p, "#<scanner:%p>", x);
}
void scanner_free(scanner *x) {
    leaf_free((leaf_object*)x->b);
    leaf_free((leaf_object*)x->p);
    free(x);
}

LEAF_SIMPLE_TYPE(scanner)

static int next_getc(scanner *x) {
    int c = port_getc(x->p);

    if ('\n' == c) {x->line++; x->col=1;}
    else {x->col++;}

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
            if (EOF == c) return c;
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
        if (EOF == c) return set_token(x, tag);
        if (char_in(c, "()\',`#;\"") || isspace(c)) {
            port_ungetc(x->p, c);
            return set_token(x, tag);
        }
        save(x, c);
    }
}

// not 100% well-defined (i.e. can contain names which can contain brackets..)

static void scanner_get_bracket_hash(scanner *x) {
    int c, depth = 1;
    for (;;) {
        c = next_getc(x);
        if (c == '>') {
            depth--;
            if (0 == depth) {
                return set_token(x, TOK_HASH);
            }
        }
        if (c == '<') { depth++; }
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
    case '(':  set_token(x, TOK_VLEFT); break;
    case 'f':  set_token(x, TOK_FALSE); break;
    case 't':  set_token(x, TOK_TRUE); break;
    case '\\': scanner_get_atom(x, TOK_CHAR); break;
    case 'x':  scanner_get_atom(x, TOK_HEX_NUMBER); break;
    case '<':  scanner_get_bracket_hash(x); break;
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
            if (EOF == c) {
                reset(x);
                set_token(x, TOK_EOF);
                return;
            }
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
    case EOF:  return make_0token(x, TOK_EOF);
    case '\'': return make_0token(x, TOK_QUOTE);
    case '`':  return make_0token(x, TOK_QUASI_QUOTE);
    case ',': {
        c = next_getc(x);
        if ('@' == c) return make_0token(x, TOK_UNQUOTE_SPLICING);
        port_ungetc(x->p, c);
        return make_0token(x, TOK_UNQUOTE);
    }
    case '(':  return make_0token(x, TOK_LEFT);
    case ')':  return make_0token(x, TOK_RIGHT);
    case '#':  return scanner_get_hash(x);
    case '"':  return scanner_get_string(x);
    case '-':
        c = next_getc(x);
        if (isdigit(c)){
            save(x, c);
            return scanner_get_atom(x, TOK_NUMBER);
        }
        port_ungetc(x->p, c);
        break;
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


scanner *scanner_new(port *p) {
    scanner *x = calloc(1, sizeof(*x));
    leaf_init((leaf_object*)x, scanner_type());
    x->p = p;
    x->b = bytes_new(100);
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
