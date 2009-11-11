#include "ex.h"
#include "ex.h_ex_prims"


/* Reader */
// FIXME: make this thread-local when pthreads are supported. (like task.c)
static ex *thread_local_ex = NULL;
static _ ob = NIL;
static port *p = NULL;
#undef EX
#undef CONS
#define EX thread_local_ex
#define YYSTYPE object
// #define YY_DEBUG
#define SQUARE(x) x
#define CONS(car,cdr) EX->make_pair(EX, car, cdr)
#define NUMBER integer_to_object
// FIXME: this is in-band data!
#define JUNK(str) CONS(SYMBOL("__parse-junk__"), STRING(str))
#define QUOTE(ob) CONS(SYMBOL("quote"), CONS(ob, NIL))
#define UNQUOTE(ob) CONS(SYMBOL("unquote"), CONS(ob, NIL))
#define UNQUOTE_SPLICING(ob) CONS(SYMBOL("unquote-splicing"), CONS(ob, NIL))
#define QUASIQUOTE(ob) CONS(SYMBOL("quasiquote"), CONS(ob, NIL))


// #define CHAR(c) STRING(c)

#define YY_INPUT(buf, result, max_size)                          \
    {                                                            \
        int yyc= port_getc(p);                                   \
        result= (EOF == yyc) ? 0 : (*(buf)= yyc, 1);             \
    }


static inline void* _realloc(void *mem, size_t size) {
    fprintf(stderr, "realloc %p %d\n", mem, (int)size);
    return realloc(mem, size);
}

#include "sexp.h_leg"

_ _ex_read(ex *ex, port *input_port) {
    p = input_port;
    EX = ex;
    ob = EOF_OBJECT;
    yyparse();
    // if (FALSE == ob) ERROR("parse", FALSE);
    
    pair *_p;
    symbol *_s;

    if ((_p = object_to_pair(ob)) && 
        (_s = object_to_symbol(_p->car, ex)) &&
        !(strcmp("__parse-junk__", _s->name))) {
        ERROR("eof", _p->cdr);
    }

    return ob;

}

