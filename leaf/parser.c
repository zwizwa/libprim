/* Scheme-style s-expression parser built on top of the scanner.
   All constructors are parameterized. */

#include <leaf/scanner.h>
#include <leaf/symbol.h>
#include <leaf/tree.h>
#include <string.h>

typedef struct _parser parser;
typedef void* (*parser_atom)(void *x, const bytes*);
typedef void* (*parser_symbol)(void *x, const char*);
typedef void* (*parser_make_0)(void *x);
typedef void* (*parser_make_2)(void *x, void *a1, void *a2);

struct _parser {
    scanner* s;
    void *x; // delegate

    parser_atom atom;
    parser_make_2 cons;
    parser_make_0 nil;
};

/* Default representation is in terms of leaf objects + trees.  Each
   record is tagged with a symbol in the first position. */

static void *dflt_nil(void *x) { 
    tree *t = tree_new(1);
    t->slot[0] = (leaf_object*)symbol_from_cstring("nil"); 
    return t;
}
static void *dflt_atom(void *x, const bytes *b) {
    tree *t = tree_new(2);

    /* Create a new bytes object with stripped token. */
    bytes *s = bytes_new(b->size);
    s->size = b->size - 1;
    memcpy(s->bytes, b->bytes + 1, b->size);
    t->slot[1] = (leaf_object*)s;

    const char *tag;
    switch(b->bytes[0]) {
    case TOK_STRING: tag = "string"; break;
    case TOK_NUMBER: tag = "number"; break;
    case TOK_SYMBOL: tag = "symbol"; break;
    default:         tag = "error"; break;
    }
    t->slot[0] = (leaf_object*)symbol_from_cstring(tag);
    return t;
}

static void *dflt_cons(void *x, void *car, void *cdr) {
    tree *t = tree_new(3);
    t->slot[0] = (leaf_object*)symbol_from_cstring("cons");
    t->slot[1] = car;
    t->slot[2] = cdr;
    return t;
}



void *parser_read(parser *p);
static void *read_tail(parser *p);

static void *read_tagged(parser *p, const bytes *token) {
    void *payload = parser_read(p);
    return p->cons(p->x, p->atom(p->x, token),
           p->cons(p->x, payload,
           p->nil(p->x)));
}

static bytes *q=NULL, *uq=NULL, *qq=NULL, *uqs=NULL, *edot=NULL, *eright=NULL;
static void parser_global_init(void) {
    if (!q) q = bytes_from_cstring(":quote");
    if (!qq) qq = bytes_from_cstring(":quasi-quote");
    if (!uq) uq = bytes_from_cstring(":unquote");
    if (!uqs) uqs = bytes_from_cstring(":unquote-splicing");
    if (!edot) edot = bytes_from_cstring("?.");
    if (!eright) edot = bytes_from_cstring("?)");
}
static void *make_atom(parser *p, const bytes *tok) {
    switch(tok->bytes[0]) {
    case TOK_QUOTE:             return read_tagged(p, q);
    case TOK_UNQUOTE:           return read_tagged(p, uq);
    case TOK_QUASI_QUOTE:       return read_tagged(p, qq);
    case TOK_UNQUOTE_SPLICING:  return read_tagged(p, uqs);
    default: return p->atom(p->x, tok);
    }
}

static void *make_any(parser *p, const bytes *tok) {
    switch(tok->bytes[0]) {
    case TOK_LEFT:
    {
        void *car = parser_read(p);
        void *cdr = read_tail(p);
        return p->cons(p->x, car, cdr);
    }
    case TOK_RIGHT: return p->atom(p->x, eright);
    case TOK_DOT:   return p->atom(p->x, edot);
    default:        return make_atom(p, tok);
    }
}

static void *read_tail(parser *p) {
    scanner_read(p->s);
    const bytes *tok = scanner_token(p->s);
    switch(tok->bytes[0]) {
    case TOK_DOT:  return parser_read(p);
    case TOK_LEFT: return p->nil(p->x);
    default:
    {
        void *car = make_any(p, tok);
        void *cdr = read_tail(p);
        return p->cons(p->x, car, cdr);
    }
    }
}

void *parser_read(parser *p) {
    parser_global_init();
    scanner_read(p->s);
    const bytes *tok = scanner_token(p->s);
    return make_any(p, tok);
}


parser *parser_new(port *prt) {
    parser *p = calloc(1,sizeof(*p));
    p->s = scanner_new(prt);
    p->x = NULL;
    p->atom = dflt_atom;
    p->cons = dflt_cons;
    p->nil  = dflt_nil;
}

#ifdef _PARSER_TEST_
int main(void) {
    parser *par = parser_new(port_file_new(stdin, "<stdin>"));
    leaf_object *o = (leaf_object*)parser_read(p);
    // print leaf object
    return 0;
}
#endif
