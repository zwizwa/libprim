/* Scheme-style s-expression parser built on top of the scanner.
   All constructors are parameterized. */

typedef struct _parser parser;
typedef void* (*parser_atom_1)(void *x, const char*);
typedef void* (*parser_make_0)(void *x);
typedef void* (*parser_make_2)(void *x, void *a1, void *a2);

struct _parser {
    scanner* s;
    void *x; // delegate

    parser_atom_1 symbol;
    parser_atom_1 number;
    parser_atom_1 character;
    parser_atom_1 string;
    parser_atom_1 error;

    parser_make_2 cons;
    parser_make_0 nil;
};

void *parser_read(parser *p);

static void read_tagged(parser *p, const char *tag) {
    void *payload = parser_read(p);
    return p->cons(p->x, p->symbol(p->x, tag),
           p->cons(p->x, payload,
           p->nil(p->x)));
}

static void make_atom(parser *p, int tag, char *str) {
    switch(tag) {
    case TOK_ERROR:  return p->error(p->x, str);
    case TOK_STRING: return p->string(p->x, str);
    case TOK_NUMBER: return p->number(p->x, str);
    case TOK_SYMBOL: return p->symbol(p->x, str);
        //case TOK_HASH:
    case TOK_QUOTE:             return read_tagged(p, "quote");
    case TOK_UNQUOTE:           return read_tagged(p, "unquote");
    case TOK_QUASI_QUOTE:       return read_tagged(p, "quasi-quote");
    case TOK_UNQUOTE_SPLICING:  return read_tagged(p, "unquote-splicing");
    }
}

static void make_any(parser *p, int tag, const char *str) {
    switch(tag) {
    case TOK_LP:
    {
        void *car = parser_read(p);
        void *cdr = read_tail(p);
        return p->cons(p->x, car, cdr);
    }
    case TOK_RP:
    case TOK_DOT:
        return p->error(p->x, p->b->bytes);
    default:
        return make_atom(p, tag, str);
    }
}

static void read_tail(parser *p) {
    scanner_token(p->s);
    int tag = p->b->bytes[0];
    char *str = p->b->bytes + 1;
    switch(tag) {
    case TOK_DOT: return parser_read(p);
    case TOK_RP:  return p->nil(p->x);
    default:
    {
        void *car = make_any(p, tag, str);
        void *cdr = read_tail(p);
        return p->cons(p->x, car, cdr);
    }
}


void *parser_read(parser *p) {
    scanner_token(p->s);
    int tag = p->b->bytes[0];
    char *str = p->b->bytes + 1;
    return make_any(p, tag, str);
}
