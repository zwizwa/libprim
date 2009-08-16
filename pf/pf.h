#ifndef _PF_H_
#define _PF_H_

#include "ex.h"

/* STATE */

typedef struct {
    ex m;

    /* Linear memory. */
    _ ds;    // parameter stack
    _ rs;    // retain stack
    _ free;  // free list
    _ output;

    /* Graph memory. */
    _ ip;
    _ ip_halt;
    _ ip_abort;
    _ dict;

    /* Symbol cache. */
    _ s_underflow;
    _ s_eval;
    _ s_quote;

} pf;

// SUPER
#define EX    (&pf->m)
#define TYPES (EX->p)
#define GC    (EX->gc)

#include "../ex/ex_prims.h_ex_prims"

/* MEMORY */

typedef aref box;
typedef aref lin;
#define TAG_LIN   VECTOR_TAG(12)
#define TAG_BOX   VECTOR_TAG(13)
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

#define TAG_QUOTE VECTOR_TAG(14)
#define TAG_SEQ   VECTOR_TAG(15)
DEF_STRUCT(quote, TAG_QUOTE)
DEF_STRUCT(seq,   TAG_SEQ)

// The empty program.
#define NOP  CONSTANT(0x80)
#define HALT CONSTANT(0x81)


#define PF_EX_RESTART 1
#define PF_EX_ABORT 2

void _pf_push(pf *pf, _ ob);
_ _pf_make_symbol(pf *pf, const char *str);
// #define SYMBOL(str)   _pf_make_symbol(pf, str)
#define NUMBER(n)     integer_to_object(n)

void _pf_top_interpret_list(pf *pf, _ expr);
#define EVAL(expr)    _pf_top_interpret_list((pf*)EX, expr)


#endif
