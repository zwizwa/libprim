#ifndef _GC_H_
#define _GC_H_

#include "gc_config.h"

#include <stdio.h>

// GC type tagging

#define GC_TAG(x) ((x)&3)
#define GC_ATOM    0   /* pointer not managed by GC */
#define GC_VECTOR  1   /* vector pointer, managed by GC */
#define GC_INTEGER 2   /* integer number (shifted) */
#define GC_USER    3   /* unused by GC: user tag */

typedef struct _atom atom;
typedef struct _atom_class atom_class;
typedef void (*atom_free)(void *);
typedef struct _vector vector;
typedef unsigned long object;
typedef struct _gc gc;

struct _atom_class {
    atom_free free;
};

struct _atom {
    atom_class *op;
};

/* The size field in the _vector struct can be used to store
   additional tag bits.  GC_TAG_MASK tells the GC what to ignore. */
#ifndef GC_VECTOR_TAG_MASK
#define GC_VECTOR_TAG_MASK 0xFFFFFFFFFFFFFFFFL
#endif

struct _vector {
    long tag_size;
    object slot[0];
};

struct _gc {
    object roots;
    object *current;
    long slot_index;
    long slot_total;
    object *old;
};


void gc_collect(gc *gc);
object gc_alloc(gc *gc, long slots);
object gc_vector(gc *gc, long slots, ...);
gc *gc_new(long total);

static inline vector *object_vector(object ob) {
    if(GC_VECTOR == GC_TAG(ob)) { 
        ob &= 0xFFFFFFFFFFFFFFFCL; 
        return ((vector*)ob); 
    }
    else return NULL;
}

static inline atom *object_atom(object ob) {
    if (GC_ATOM == GC_TAG(ob)) return (atom*)ob;
    else return NULL;
}

#endif
