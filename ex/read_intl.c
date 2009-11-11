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
static _ _cons(parser_ctx *x, _ car, _ cdr) {return x->ex->make_pair(x->ex, car, cdr);}
static _ _atom(parser_ctx *x, const bytes *tok) {
    ex* ex = x->ex;
    const char *str = tok->bytes+1;
    switch(tok->bytes[0]) {
    case TOK_TRUE:   return TRUE;
    case TOK_FALSE:  return FALSE;
    case TOK_CHAR:   return NAMED_CHAR(str);
    case TOK_STRING: return STRING(str);
    case TOK_NUMBER: return NUMBER(atoi(str));
    case TOK_SYMBOL: return SYMBOL(str);
    }
    x->tok = tok;
    longjmp(x->jb, 1);
}

_ _ex_read(ex *ex, port *input_port) {
    parser_ctx x = {ex, NULL};
    parser *p = parser_new(input_port);
    p->ctx  = &x;
    p->cons = (parser_cons)_cons;
    p->atom = (parser_atom)_atom;
    p->nil  = (parser_ob)_nil;
    p->eof  = (parser_ob)_eof;

    _ ob = NIL;
    if (!setjmp(x.jb)) {
        ob = (object)parser_read(p);
    }
    if (x.tok) {
        return ERROR("parse", SYMBOL(x.tok->bytes));
    }
    // _ex_write(ex, ob);
    return ob;
}

