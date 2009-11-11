#include "ex.h"
#include "ex.h_ex_prims"

#include <leaf/parser.h>

#define NUMBER integer_to_object
#define EX ex

static _ _nil(ex *ex) { return NIL; }
static _ _atom(ex *ex, const bytes *tok) {
    const char *str = tok->bytes+1;
    switch(tok->bytes[0]) {
    case TOK_CHAR:   return CHAR(str);
    case TOK_STRING: return STRING(str);
    case TOK_NUMBER: return NUMBER(atoi(str));
    case TOK_SYMBOL: return SYMBOL(str);
    default:         return CONS(SYMBOL("error"), STRING(tok->bytes));
    }
}

_ _ex_read(ex *ex, port *input_port) {
    parser *p = parser_new(input_port);
    p->ctx  = ex;
    p->cons = (parser_cons)ex_cons;
    p->nil  = (parser_nil)_nil;
    p->atom = (parser_atom)_atom;
    
    _ ob = (object)parser_read(p);
    parser_free(p);
    return ob;
}

