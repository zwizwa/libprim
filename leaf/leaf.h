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


#ifndef __LEAF_H__
#define __LEAF_H__

#include <stdio.h>

/* Because of cross-deps between the objects, some are declared here. */
typedef struct _port port;
typedef struct _bytes bytes;


typedef struct _leaf_class leaf_class;
typedef struct _leaf_object leaf_object;

typedef void (*leaf_free_m)(leaf_object *);
typedef int (*leaf_write_m)(leaf_object *, port *);

// a class is a collection of methods
struct _leaf_class {
    leaf_free_m free;
    leaf_write_m write;
    leaf_write_m dump;    // don't write metadata
};

// an object refers to its class
struct _leaf_object {
    leaf_class *methods;
};

void leaf_free(leaf_object *x);
int leaf_write(leaf_object *x, port *p);



#define LEAF_SIMPLE_TYPE(name) \
    name##_class *name##_type(void) { \
    static name##_class *x = NULL; \
    if (!x) {\
    x = calloc(1, sizeof(*x)); \
    x->super.free = (leaf_free_m)name##_free; \
    x->super.write = (leaf_write_m)name##_write; \
    } return x; }



#endif
