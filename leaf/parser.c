/* Scheme-style s-expression parser built on top of the scanner.
   All constructors are parameterized. */

#include <leaf/parser.h>
#include <leaf/symbol.h>
#include <leaf/tuple.h>
#include <string.h>
#include <stdlib.h>


/* Default representation is in terms of leaf objects + tuples.  Each
   record is tagged with a symbol in the first position. */

static void *dflt_nil(void *x) { 
    tuple *t = tuple_new(1);
    t->slot[0] = (leaf_object*)symbol_from_cstring("nil"); 
    return t;
}
static void *dflt_eof(void *x) { return NULL; }
static void *dflt_atom(void *x, const bytes *b) {
    tuple *t = tuple_new(2);

    /* Create a new bytes object with stripped token. */
    bytes *s = bytes_new(b->size);
    s->size = b->size - 1;
    memcpy(s->bytes, b->bytes + 1, b->size);
    t->slot[1] = (leaf_object*)s;

    const char *tag;
    switch(b->bytes[0]) {
    case TOK_CHAR:       tag = "char"; break;
    case TOK_STRING:     tag = "string"; break;
    case TOK_NUMBER:     tag = "number"; break;
    case TOK_HEX_NUMBER: tag = "hex-number"; break;
    case TOK_SYMBOL:     tag = "symbol"; break;
    case TOK_HASH:       tag = "obj"; break; // (1)
    default:             tag = "error"; break;
    }
    t->slot[0] = (leaf_object*)symbol_from_cstring(tag);
    return t;

    /* (1) As long as representations of un-printable objects like
       "#<void>" respect balanced '<' and '>' characters, they are
       scanned as a single token.  In SC and PF reading these tokens
       is an error, while the default parser tags them as "obj", which
       is useful to make s-expression communication a bit more rebust
       (an unprintable value won't mess up the channel sync).  */

}

static void *dflt_cons(void *x, void *car, void *cdr) {
    tuple *t = tuple_new(3);
    t->slot[0] = (leaf_object*)symbol_from_cstring("cons");
    t->slot[1] = car;
    t->slot[2] = cdr;
    return t;
}

/* Note that this _first_ constructs a list using the CONS provided,
   and then passes the result of that to this function. */

static void *dflt_vector(void *x, void *lst) {
    tuple *t = tuple_new(2);
    t->slot[0] = (leaf_object*)symbol_from_cstring("vector");
    t->slot[1] = lst;
    return t;
}


void *parser_read(parser *p);
static void *read_tail(parser *p, int allow_dot);
static void *read_list(parser *p, int allow_dot);

static void *read_tagged(parser *p, const bytes *token) {
    void *payload = parser_read(p);
    return p->cons(p->ctx, p->atom(p->ctx, token),
           p->cons(p->ctx, payload,
           p->nil(p->ctx)));
}

static bytes *q=NULL, *uq=NULL, *qq=NULL, *uqs=NULL,
    *edot=NULL, *eright=NULL, *eeof=NULL;
static void parser_global_init(void) {
    if (!q) q = bytes_from_cstring(":quote");
    if (!qq) qq = bytes_from_cstring(":quasiquote");
    if (!uq) uq = bytes_from_cstring(":unquote");
    if (!uqs) uqs = bytes_from_cstring(":unquote-splicing");
    if (!edot) edot = bytes_from_cstring("?.");
    if (!eright) eright = bytes_from_cstring("?)");
    if (!eeof) eeof = bytes_from_cstring("?EOF");
}
static void *make_atom(parser *p, const bytes *tok) {
    switch(tok->bytes[0]) {
    case TOK_QUOTE:             return read_tagged(p, q);
    case TOK_UNQUOTE:           return read_tagged(p, uq);
    case TOK_QUASI_QUOTE:       return read_tagged(p, qq);
    case TOK_UNQUOTE_SPLICING:  return read_tagged(p, uqs);
    default: return p->atom(p->ctx, tok);
    }
}

static const bytes *next(parser *p) {
    scanner_read(p->s);
    const bytes *tok = scanner_token(p->s);
    // fprintf(stderr, "TOK: %s\n", tok->bytes);
    return tok;
}

static void *make_any(parser *p, const bytes *tok) {
    switch(tok->bytes[0]) {
    case TOK_EOF:   return p->eof(p->ctx);
    case TOK_VLEFT: return p->vector(p->ctx, read_list(p, 0));
    case TOK_LEFT:  return read_list(p, 1);
    case TOK_RIGHT: return p->atom(p->ctx, eright);
    case TOK_DOT:   return p->atom(p->ctx, edot);
    default:        return make_atom(p, tok);
    }
}

static void *read_list(parser *p, int allow_dot) {
    const bytes *tok = next(p);
    switch(tok->bytes[0]) {
    case TOK_RIGHT: return p->nil(p->ctx);
    case TOK_DOT:   return p->atom(p->ctx, edot);
    }
    void *car = make_any(p, tok);
    void *cdr = read_tail(p, allow_dot);
    return p->cons(p->ctx, car, cdr);
}

static void *read_tail(parser *p, int allow_dot) {
    const bytes *tok = next(p);
    switch(tok->bytes[0]) {
    case TOK_EOF:  return(p->atom(p->ctx, eright));
    case TOK_DOT:   
        if (allow_dot) {
            void *tail = parser_read(p);
            tok = next(p);
            if (TOK_RIGHT != tok->bytes[0]) {
                // cons the tail so we won't leak
                return p->cons(p->ctx, tail, p->atom(p->ctx, eright));
            }
            return tail;
        }
        else return p->atom(p->ctx, edot);
        
    case TOK_RIGHT: return p->nil(p->ctx);
    default:
    {
        void *car = make_any(p, tok);
        void *cdr = read_tail(p, allow_dot);
        return p->cons(p->ctx, car, cdr);
    }
    }
}

void *parser_read(parser *p) {
    parser_global_init();
    const bytes *tok = next(p);
    return make_any(p, tok);
}

void parser_free(parser *p) {
    scanner_free(p->s);
    free(p);
}
static int parser_write(parser *par, port *p) {
    return port_printf(p, "#<parser:%p>", par);
}
LEAF_SIMPLE_TYPE(parser)

parser *parser_new(port *prt) {
    parser *p = calloc(1,sizeof(*p));
    leaf_init((leaf_object*)p, parser_type());
    p->s = scanner_new(prt);
    p->ctx = NULL;
    p->atom = dflt_atom;
    p->cons = dflt_cons;
    p->nil  = dflt_nil;
    p->eof  = dflt_eof;
    p->vector = dflt_vector;
    return p;
}


/* De-consing standard parse result (using default_* functions).

   1. The parser creates CONS lists based on the constructors provided
      in the parser struct, ie.e .atom .cons .nil ...   For
      s-expressions this simplifies the parser.

   2. The default implementation produces leaf/tuple data structures.
      (The EX reader overrides this using GCd vector constructors).

   3. To use s-expression to embed "flat" constructors (i.e. ML-style
      ADTs) represented as tuples, it is simpler to remove one level
      of quotation.  This is what this routine does: convert CONS
      lists to flat tuples.  */

tuple *tuple_ctor(tuple *t, symbol *tag, int args) {
    if ((leaf_type(&t->base) == tuple_type()) &&
        (t->size == args+1) &&
        (t->slot[0] == (leaf_object*)tag)) {
        return t;
    }
    else return NULL;
}

/* Linear operation: reuses data, frees and re-creates if necessary. */
tuple *tuple_ast_flatten_lin(tuple *in) {

    symbol *cons = symbol_from_cstring("cons");
    symbol *nil = symbol_from_cstring("nil");
    tuple *lst;
    int n;

    /* Get length of list, or return original object if not a proper list. */
    for(n = 0, lst = in;;) {
        if (tuple_ctor(lst, nil, 0)) break;
        if (!tuple_ctor(lst, cons, 2)) return in; // improper list
        n++; 
        lst = (tuple*)(lst->slot[2]);
    }
    if (!n) return in; // empty list

    /* Create tuple and populate with list elements. */
    tuple *t = tuple_new(n);
    int i;
    for (lst=in, i=0; i<n; i++) {
        tuple *cell = tuple_ctor(lst, cons, 2);
        leaf_object *x = cell->slot[1];
        cell->slot[1] = NULL;
        t->slot[i] = (leaf_object*)tuple_ast_flatten_lin((tuple*)x);
        lst = (tuple*)(cell->slot[2]);
    }
    leaf_free((leaf_object*)in);
    return t;
}




#ifdef _PARSER_TEST_
int main(void) {
    parser *par = parser_new(port_file_new(stdin, "<stdin>"));
    leaf_object *o;
    port *out = port_file_new(stdout, "<stdout>");
    while ((o = (leaf_object*)parser_read(par))) {
        leaf_write(o, out);
        port_printf(out, "\n\n");
        // leaf_free(o);
    }
    return 0;
}
#endif
