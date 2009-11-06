/* libprim leaf object model

The libprim project is structured mainly in two parts: 

* a collection of basic objects implemented in C, on top of a minimal
  interface to promote reuse without introducing many dependencies

* code built on top of this to implement basic scripting language
  functionality like memory management and function composition (see
  Scheme and PF).

This file specifies the interface to which all C objects need to
comply.  In practice, to use code objects from external C libraries
you wrap them according to this interface.

*/


#ifndef __LEAF_CLASS_H__
#define __LEAF_CLASS_H__

#include <stdio.h>

typedef struct _leaf_class leaf_class;
typedef struct _leaf_object leaf_object;

typedef void (*leaf_free)(leaf_object *);
typedef void (*leaf_write)(leaf_object *, FILE *);

// a class is a collection of methods
struct _leaf_class {
    leaf_free free;
    leaf_write write;
};

// an object refers to its class
struct _leaf_object {
    leaf_class *methods;
};


static inline void free_leaf(leaf_object *x) {
    x->methods->free(x);
}


#endif
