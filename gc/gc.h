#ifndef _GC_H_
#define _GC_H_

#define unlikely(x) __builtin_expect((x),0)

#include <stdio.h>
#include <stdarg.h>

/* The size field in the _vector struct can be used to store
   additional tag bits.  GC_TAG_MASK tells the GC what to ignore. */

#define GC_TAG_SHIFT 2

static inline long GC_TAG(long x) { return x&((1<<GC_TAG_SHIFT)-1); }
static inline void* GC_POINTER(long x) { 
    long mask = -1;
    mask <<= GC_TAG_SHIFT;
    return (void*)(x&mask);
}

#define GC_CONST   0   /* pointer untyped, not managed */
#define GC_VECTOR  1   /*         vector,  managed */
#define GC_INTEGER 2   /* integer number (shifted) */
#define GC_FIN     3   /* finalizer */


/* Note that GC_INTEGER (which would make integer addition and
   subtraction simpler) can't be 0 when we want NIL == 0 */

typedef void (*fin)(void *);
typedef struct _vector vector;
typedef unsigned long object;
typedef long integer;
typedef struct _gc gc;

struct _vector {
    unsigned long header;  // [ struct_tags | length | GC tags ]
    object slot[0];
};

typedef void (*gc_finalize)(gc *);
typedef void (*gc_mark_roots)(void *ctx, gc_finalize finalize);

struct _gc {
    object *current;
    long    current_index;
    object *old;
    long    old_index;
    long    slot_total;
    gc_mark_roots mark_roots; // (1)
    void *mark_roots_ctx;
};

/* Client is free to abort the C stack during the execution of (1) but
   has to call the finalize method after performing gc_mark() for the
   root objects.

   Aborting is useful if the C stack contains references to objects
   that are not accessible from the root.  These will be invalid after
   the GC finishes.

   If the GC is part of an interpreter written in functional style it
   is easiest to just abort the current step and start over after
   collection.

   If all the references on the C stack are reachable from the root
   pointers, it is ok to let (1) return such that the allocation that
   triggered the GC will continue.  Most likely this is not the case..
   Even gc_vector() already messes things up. */




gc *gc_new(long total, gc_mark_roots fn, void *ctx);
void gc_collect(gc *gc);
object gc_mark(gc *gc, object o_old);


/* Conversion from tagged objects to one of the 4 C data types.  When
   the tag doesn't match, NULL is returned.  Conversion to integer
   always succeeds. (FIXME: make this word index size, i.e. on 64 bits
   use GC_TAG_SHIFT = 3)
*/

static inline vector *object_to_vector(object ob) {
    if(GC_VECTOR == GC_TAG(ob)) return (vector*)GC_POINTER(ob);
    else return NULL;
}

/* Note: this is a _pointer_ to fin.  Code isn't always aligned to
   accomodate GC tag bits, but data alignment can be enforced, plus
   malloc() already returns aligned data.  Embedding the code pointer
   in a data structure seems to be the right approach. */
static inline fin *object_to_fin(object ob) {
    if (GC_FIN == GC_TAG(ob)) return (fin*)GC_POINTER(ob);
    else return NULL;
}
static inline void *object_to_const(object ob) {
    if (GC_CONST == GC_TAG(ob)) return (void*)GC_POINTER(ob);
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
static inline object fin_to_object(fin *f) {
    return ((object)f) + GC_FIN;
}
static inline object const_to_object(void *c) {
    return ((object)c) + GC_CONST;
}
static inline object integer_to_object(long i) {
    return (object)(GC_INTEGER + (i << GC_TAG_SHIFT));
}


static inline long vector_size(vector *v) {
    return object_to_vector_size(v->header);
}


/* Basic allocation functions are inline. */
int gc_grow(gc *gc, long add_slots);
static inline int gc_full(gc *gc, int slots) {
    return (gc->current_index + slots) >= gc->slot_total;
}
/* User must fill the allocated space with valid tagged values before
   calling gc_alloc again. */
void gc_when_full(gc *gc, long size);
static inline vector *gc_alloc(gc *gc, long size) {
    long slots = size + 1;
    if (unlikely(gc_full(gc, slots))) {
        gc_when_full(gc, slots);
    }
    vector *v = (vector *)(&gc->current[gc->current_index]);
    v->header = integer_to_object(size);
    gc->current_index += slots;
    return v;
}

#if 1
static inline object gc_make_v(gc *gc, long slots, va_list ap) {
    vector *v = gc_alloc(gc, slots);
    long i = 0;
    for (i=0; i<slots; i++) {
        v->slot[i] = va_arg(ap, object);
    }
    return vector_to_object(v);
}
static inline object gc_make(gc *gc, long slots, ...) {
    va_list ap;
    va_start(ap, slots);
    object o = gc_make_v(gc, slots, ap);
    va_end(ap);   
    return o;
}
#endif


#endif
