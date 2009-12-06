#include "ex.h"
#include "ex.h_ex_prims"

#include <leaf/parser.h>

#define NUMBER integer_to_object
#define EX ex

typedef struct {
    ex* ex;
    const bytes *tok;
    jmp_buf jb;
} parser_ctx;

static _ _nil(void) { return NIL; }
static _ _eof(void) { return EOF_OBJECT; }
static _ _cons(parser_ctx *x, _ car, _ cdr) {
    return x->ex->make_pair(x->ex, car, cdr);
}
static _ _vector(parser_ctx *x, _ lst) {
    return ex_list_to_vector(x->ex, lst);
}
static _ _atom(parser_ctx *x, const bytes *tok) {
    int base = 0;
    ex* ex = x->ex;
    const char *str = tok->bytes+1;
    switch(tok->bytes[0]) {
    case TOK_TRUE:   return TRUE;
    case TOK_FALSE:  return FALSE;
    case TOK_CHAR:   return NAMED_CHAR(str);
    case TOK_STRING: return STRING(str);
    case TOK_HEX_NUMBER:
        base = 16;
    case TOK_NUMBER: {
        if (!base) base = 10;
        char *end = NULL;
        long long int l = 0;
        double d = 0;
        int i;
        int decimal = 0;
        for(i=0; str[i]; i++) {if (str[i] == '.') { decimal = 1;  break; }}
        if (decimal) 
            { d = strtod(str, &end); if (end[0]) goto error; return INEXACT(d); }
        else
            { l = strtoll(str, &end, base); if (end[0]) goto error; return NUMBER(l); }
    }
    case TOK_SYMBOL: return SYMBOL(str);
    }
  error:
    x->tok = tok;
    longjmp(x->jb, 1);
}

_ _ex_read(ex *ex, port *input_port) {
    parser_ctx x = {ex, NULL};
    parser *p = parser_new(leaf_dup((leaf_object*)input_port));
    p->ctx  = &x;
    p->cons = (parser_cons)_cons;
    p->atom = (parser_atom)_atom;
    p->nil = (parser_ob)_nil;
    p->eof = (parser_ob)_eof;
    p->vector = (parser_vector)_vector;

    _ ob = NIL;
    if (!setjmp(x.jb)) {
        ob = (object)parser_read(p);
    }
    leaf_free((leaf_object*)p);
    if (x.tok) {
        return ERROR("parse", SYMBOL(x.tok->bytes));
    }
    else {
        return ob;
    }
}

