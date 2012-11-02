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
typedef struct _leaf_ctx leaf_ctx;


typedef struct _leaf_class leaf_class;
typedef struct _leaf_object leaf_object;

#define LEAF_CLASS(super) leaf_class super

typedef void (*leaf_free_m)(leaf_object *);
typedef int (*leaf_write_m)(leaf_object *, port *);


struct _leaf_class {
    /* Recursively free resources that belong to this instance. */
    leaf_free_m _free;

    /* Write out a human-readable and possible machine readable
       representation.  If the object cannot be serialized by the
       standard Scheme parser, make sure this produces a string of the
       form "#<...>" where the dots are replaced with any string that
       contains an equal number of '<' and '>' characters.  This makes
       sure the standard scanner can at least tokenize the data. */
    leaf_write_m _write;

    /* Dump raw data without metadata. */
    leaf_write_m dump;
};

/* FIXME: make class access abstract also. */
static inline void
leaf_class_init(leaf_class *t,
                leaf_free_m free,
                leaf_write_m write) {
    t->_free = free;
    t->_write = write;

    t->dump = NULL;
}


/* Reference counts.  I've been debating whether each object should
   have an RC, as it is not used in GCd languages.

   The answer is yes for the following reasons:
   
     - explicitly wrapping objects in an RC struct is cumbersome

     - linearity (RC always == 1) is too great a constraint on
       composite leaf objects.

     - an embedded RC allows data to travel between a RC-based memory
       manager and a GC-based one (i.e. Scheme <-> PF) without extra
       effort.
*/

struct _leaf_object {
    leaf_class *__type;  /* Behaviour */
    int _rc;            /* Nb. of users */
};

/* Thea idea is to keep this abstract in all the other objects such
   that the representation can be patched */
#define LEAF_OBJECT(x) leaf_object x

static inline void leaf_init(leaf_object *o, leaf_class *type) {
    o->__type = type;
    o->_rc = 1;
}
static inline leaf_class *leaf_type(leaf_object *o) { return o->__type; }
static inline int leaf_rc(leaf_object *o) { return o->_rc; }
static inline void leaf_rc_dec(leaf_object *o) { o->_rc--; }
static inline leaf_object *leaf_dup(leaf_object *o) { o->_rc++; return o; }
#define LEAF_DUP(ob) ((typeof(ob))(leaf_dup((leaf_object*)ob)))


/* If RC=1 this will effectively call the free method. */
void leaf_free(leaf_object *x);


int leaf_write(leaf_object *x, port *p);

#define LEAF_SIMPLE_TYPE(name) \
    leaf_class *name##_type(void) { \
    static name##_class *x = NULL; \
    if (!x) {\
    x = calloc(1, sizeof(*x)); \
    leaf_class_init((leaf_class*)x, \
                    (leaf_free_m)name##_free, \
                    (leaf_write_m)name##_write);        \
    } return (leaf_class*)x; }



typedef int (*leaf_predicate)(leaf_object *o, void *ctx);


#endif
