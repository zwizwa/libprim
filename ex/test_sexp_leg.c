/* Example */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum stx_tag {
    t_junk,
    t_eof, t_false, t_true, t_nil,
    t_qstring, t_symbol, t_number, t_char, t_named_char, 
    t_cons, t_square, t_quote, t_quasiquote, t_unquote, t_unquote_splicing,
};
struct stx {
    enum stx_tag t;
    void *a;
    void *b;
};

struct stx *make_tag2(enum stx_tag t, void *a, void *b) {
    struct stx *x = malloc(sizeof(*x));
    x->t = t; x->a = a; x->b = b;
    return x;
}
char* copy_string(const char *x) {
    char *s = malloc(1+strlen(x));
    strcpy(s,x);
    return s;
}

#define TAG2        make_tag2
#define TAG1(t,a)   TAG2(t,a,NULL)
#define TAG0(t)     TAG1(t,NULL)

#define STR copy_string

#define EOF_OBJECT          TAG0(t_eof)
#define FALSE               TAG0(t_false)
#define TRUE                TAG0(t_true)
#define NIL                 TAG0(t_nil)

#define JUNK(x)             TAG1(t_junk,x)
#define QSTRING(x)          TAG1(t_qstring,STR(x))
#define SYMBOL(x)           TAG1(t_symbol,STR(x))
#define INTEGER(x)          TAG1(t_number,STR(x))
#define CHAR(x)             TAG1(t_char,STR(x))
#define NAMED_CHAR(x)       TAG1(t_named_char,STR(x))

#define CONS(x,y)           TAG2(t_cons,x,y)
#define QUASIQUOTE(x)       TAG1(t_quasiquote,x)
#define QUOTE(x)            TAG1(t_quote,x)
#define UNQUOTE(x)          TAG1(t_unquote,x)
#define UNQUOTE_SPLICING(x) TAG1(t_unquote_splicing,x)
#define SQUARE(x)           TAG1(t_square,x)


#define YYSTYPE struct stx*
struct stx *ob;

void print_stx(struct stx *stx);
void print_stx_tail(struct stx *stx) {
  next:
    if (stx->t == t_cons) {
        printf(" ");
        print_stx(stx->a);
        stx = stx->b;
        goto next;
    }
    else {
        printf(" . ");
        print_stx(stx);
    }
}
void print_stx(struct stx *stx) {
    switch(stx->t) {
        /* Atoms */
    case t_junk:    printf("(JUNK %s)", (char*)stx->a); break;
    case t_eof:     printf("#EOF"); break;
    case t_false:   printf("#f"); break;
    case t_true:    printf("#t"); break;
    case t_nil:     printf("()"); break;
    case t_qstring: printf("\"%s\"", (char*)stx->a); break;
    case t_number:
    case t_char:
    case t_named_char:
    case t_symbol:  printf("%s", (char*)stx->a); break;
        /* Structures */
    case t_square:
        print_stx(stx->a);
        break;
    case t_cons:
        printf("(");
        print_stx(stx->a);
        print_stx_tail(stx->b);
        printf(")");
        break;
    case t_quote:
        printf("'");
        print_stx(stx->a);
        break;
    case t_quasiquote:
        printf("`");
        print_stx(stx->a);
        break;
    case t_unquote:
        printf(",");
        print_stx(stx->a);
        break;
    case t_unquote_splicing:
        printf(",@");
        print_stx(stx->a);
        break;
    }
}

#include "sexp.leg.h"
int main(void) {
    yyparse();
    print_stx(ob);
    return 0;
}
