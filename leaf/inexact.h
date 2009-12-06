/* Inexact number class. */


#ifndef _LEAF_INEXACT_H_
#define _LEAF_INEXACT_H_

#include <leaf/leaf.h>
#include <math.h>

typedef struct {
    leaf_class super;
} inexact_class;

typedef struct {
    leaf_object base;
    double value;
} inexact;

inexact *inexact_new(double f);
leaf_class *inexact_type(void);


#define DEF_INEXACT_BINOP(name,op) static inline void inexact_##name \
    (inexact *a, inexact *b, inexact *c) \
    {c->value = a->value op b->value;}

#define DEF_INEXACT_UNOP(name,op) static inline void inexact_##name \
    (inexact *a, inexact *b) \
    {b->value = op(a->value);}

DEF_INEXACT_BINOP(add, +)
DEF_INEXACT_BINOP(sub, -)
DEF_INEXACT_BINOP(div, /)
DEF_INEXACT_BINOP(mul, *)

DEF_INEXACT_UNOP(sin, sin)
DEF_INEXACT_UNOP(cos, cos)
DEF_INEXACT_UNOP(tan, tan)

DEF_INEXACT_UNOP(asin, asin)
DEF_INEXACT_UNOP(acos, acos)
DEF_INEXACT_UNOP(atan, atan)

DEF_INEXACT_UNOP(exp, exp)
DEF_INEXACT_UNOP(log, log)

DEF_INEXACT_UNOP(sqrt, sqrt)



#endif
