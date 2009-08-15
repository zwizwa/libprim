#ifndef _PF_H_
#define _PF_H_

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
    _ dict;

} pf;

#include "pf2.h_"

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



#endif
