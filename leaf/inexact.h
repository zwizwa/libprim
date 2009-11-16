/* Inexact number class. */


#ifndef _LEAF_INEXACT_H_
#define _LEAF_INEXACT_H_

#include <leaf/leaf.h>

typedef struct {
    leaf_class super;
} inexact_class;

typedef struct {
    inexact_class *type;
    double value;
} inexact;

inexact *inexact_new(double f);
inexact_class *inexact_type(void);

#endif
