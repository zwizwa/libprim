#ifndef _PF_H_
#define _PF_H_

#include "ex.h"

/* STATE */

typedef struct {
    mem m;

    /* Linear memory. */
    _ ds;    // parameter stack
    _ rs;    // retain stack
    _ free;  // free list
    _ output;

    /* Graph memory. */
    _ ip;
    _ ip_abort;
    _ dict;

    /* Symbol cache. */
    _ s_underflow;
    _ s_eval;

} pf;

#include "pf.h_"

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
    _ sub;
    _ next;
} code;

#define TAG_QUOTE VECTOR_TAG(14)
#define TAG_CODE  VECTOR_TAG(15)
DEF_STRUCT(quote, TAG_QUOTE)
DEF_STRUCT(code,  TAG_CODE)

static inline _ _pf_quote(pf *pf, _ data) { 
    return gc_make_tagged(pf->m.gc, TAG_QUOTE, 1, data);
}
static inline _ _pf_code(pf *pf, _ sub, _ next) { 
    return gc_make_tagged(pf->m.gc, TAG_CODE, 2, sub, next);
}

#define TYPES pf->m.p
#define GC    pf->m.gc

#define PF_EX_RESTART 1
#define PF_EX_ABORT 2

void _pf_push(pf *pf, _ ob);
_ _pf_make_symbol(pf *pf, const char *str);
#define SYMBOL(str)   _pf_make_symbol(pf, str)
#define NUMBER(n)     integer_to_object(n)

#endif
