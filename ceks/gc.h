#ifndef _GC_H_
#define _GC_H_

#include "gc_config.h"

#include <stdio.h>

// GC type tagging

#define GC_TAG_SHIFT 2
#define GC_TAG(x)     ((x)&((1<<GC_TAG_SHIFT)-1))
#define GC_POINTER(x) ((x)&(0xFFFFFFFFFFFFFFFFL<<GC_TAG_SHIFT))

#define GC_CONST   0   /* (unsafe) untyped pointer not managed by GC */
#define GC_ATOM    1   /* object pointer, finalized by GC */
#define GC_VECTOR  2   /* vector pointer, managed by GC */
#define GC_INTEGER 3   /* integer number (shifted) */


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

typedef void (*gc_notify)(void *);

struct _gc {
    object roots;
    object *current;
    long slot_index;
    long slot_total;
    object *old;
    gc_notify notify_fn;
    void *notify_ctx;
};


void gc_collect(gc *gc);
object gc_alloc(gc *gc, long slots);
object gc_vector(gc *gc, long slots, ...);
gc *gc_new(long total, gc_notify fn, void *ctx);


/* Conversion from tagged objects to one of the 4 C data types.  When
   the tag doesn't match, NULL is returned.  Conversion to integer
   always succeeds. (FIXME: make this word index size, i.e. on 64 bits
   use GC_TAG_SHIFT = 3)
*/

static inline vector *object_to_vector(object ob) {
    if(GC_VECTOR == GC_TAG(ob)) return (vector*)GC_POINTER(ob);
    else return NULL;
}
static inline atom *object_to_atom(object ob) {
    if (GC_ATOM == GC_TAG(ob)) return (atom*)GC_POINTER(ob);
    else return NULL;
}
static inline void *object_to_const(object ob) {
    if (GC_CONST == GC_TAG(ob)) return (atom*)GC_POINTER(ob);
    else return NULL;
}
static inline long object_to_integer(object o) { 
    return (o >> GC_TAG_SHIFT);
}

/* Vector size field has room for additional tag bits. */
static inline long object_to_vector_size(object o) { 
    return object_to_integer(o & GC_VECTOR_TAG_MASK); 
}



/* C -> tagged objects */
static inline object vector_to_object(vector *v) {
    return ((object)v) + GC_VECTOR;
}
static inline object atom_to_object(atom *a) {
    return ((object)a) + GC_ATOM;
}
static inline object const_to_object(void *c) {
    return ((object)c) + GC_CONST;
}
static inline object integer_to_object(long i) {
    return (object)(GC_INTEGER + (i << GC_TAG_SHIFT));
}


#endif
