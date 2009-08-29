#ifndef _PF_H_
#define _PF_H_

#include "ex.h"

/* STATE */

typedef struct {
    ex m;

    /* Linear memory. */
    _ p;    // parameter stack
    _ k;    // continuation
    _ freelist;
    _ output;

    /* Graph memory. */
    // _ ip;
    _ ip_repl;
    _ ip_abort;
    _ ip_prompt_tag;
    _ ip_nop;
    _ dict;

    /* Symbol cache. */
    _ s_underflow;
    _ s_quote;
    _ s_var;

} pf;

// SUPER
#define EX    (&pf->m)
#define TYPES (EX->p)
#define GC    (EX->gc)

#include "../ex/ex.h_ex_prims"

/* MEMORY */

typedef aref box;
typedef aref lin;
DEF_STRUCT(lin,   TAG_LIN)
DEF_STRUCT(box,   TAG_BOX)

/* CODE */

typedef struct {
    vector v;
    _ object;
} quote;
typedef struct {
    vector v;
    _ now;
    _ next;
} seq;

DEF_STRUCT(quote, TAG_QUOTE)
DEF_STRUCT(seq,   TAG_SEQ)

// The empty program.
#define NOP    CONSTANT(0x80)
#define HALT   CONSTANT(0x81)

// void _px_push(pf *pf, _ ob);
// _ _px_make_symbol(pf *pf, const char *str);
// #define SYMBOL(str)   _px_make_symbol(pf, str)
#define NUMBER(n)     integer_to_object(n)

void _px_interpret_list(pf *pf, _ expr);
#define EVAL(expr)    _px_top_interpret_list((pf*)EX, expr)


#define PUSH_K_NEXT(x)  pf->k = LINEAR_NEXT((x), pf->k)
#define PUSH_K_DATA(x)  pf->k = LINEAR_DATA((x), pf->k)
#define DROP_K()   _px_drop(pf, &pf->k)

#define PUSH_P(x)  _px_push(pf, (x))

#endif
