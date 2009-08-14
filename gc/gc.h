#ifndef _GC_H_
#define _GC_H_

/* Simple stop-and-copy GC for allocating graphs of vectors and atoms.

   The GC uses the following annotations:

   1. VECTOR-SIZE - For a live object, the first element of a struct
      vector_ contains the size.

   2. VECTORS? - Cells are tagged using 2 tag bits.

   3. FINALIZERS - A finalizer on heap location n is a function
      that will be applied to the constant on heap location n+1
      whenever the vector containing the finalizer is no longer
      reachable.

   4. MOVED? - A moved object contains an an object reference in the
      size slot.

   GC is implemented in a header file, since its code depends on
   configuration that changes the vector tags. 

   Note that using this GC in conjuction with C code requires solution
   of 2 problems: pointers will have changed + C stack isn't scanned.
   This keeps the implementation of the GC simple.

   In the Scheme interpreter this is solved by simply restarting each
   primitive after a collection, and making sure that this is possible
   by using purely functional primitives, or always performing side
   effects _after_ allocation.

*/



#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "object.h"

/* Note that GC_INTEGER (which would make integer addition and
   subtraction simpler) can't be 0 when we want NIL == 0 */

typedef struct _gc gc;

typedef void (*gc_finalize)(gc *);
typedef void (*gc_mark_roots)(void *ctx, gc_finalize finalize);
typedef void (*gc_overflow)(void *ctx, long nb_extra);

struct _gc {
    vector  *current;
    long    current_index;
    vector  *old;
    long    old_index;
    long    slot_total;
    long    want;             // last data request
    gc_mark_roots mark_roots; // (1)
    gc_overflow   overflow;
    void *client_ctx;
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



static gc *gc_new(long total, void *ctx, gc_mark_roots mark, 
                  gc_overflow  overflow);
static void gc_collect(gc *gc);
static object gc_mark(gc *gc, object o_old);
static int gc_grow(gc *gc, long add_slots);

#define GC_CHENEY 1








/* PRIVATE + implementation */


/* Basic allocation functions are inline. */
static inline int gc_full(gc *gc, int slots) {
    return (gc->current_index + slots) >= gc->slot_total;
}
/* User must fill the allocated space with valid tagged values before
   calling gc_alloc again. */
static void _gc_when_full(gc *gc, long size);
static vector *gc_alloc(gc *gc, long size);

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


static void _gc_assert(const char *cond, const char *file, int line) {
    fprintf(stderr, "%s: %d: gc_assert(%s)\n", file, line, cond);
    kill(getpid(), SIGTRAP);
    exit(1);
}
// __FUNCTION__
#define gc_assert(x)
// #define gc_assert(x) {if (!(x)) _gc_assert(#x, __FILE__, __LINE__);}

static void _gc_fin_slots(gc *gc, object *o, long slots) {
    long i;
    for (i=0; i<slots; i++){
        fin *f;
        if ((f = object_to_fin(o[i]))) {
            o[i] = 0; // kill the finalizer
            (*f)(object_to_const(o[i+1]));
            i++;
        }
    }
}

static vector *_gc_allot(gc *gc, long size) {
    // FIXME: check GC_VECTOR_MAX_SIZE !!
    vector *v = &gc->current[gc->current_index];
    // finalize data before overwriting
    // _gc_fin_slots(gc, &v->header, size + 1);
    // allot
    v->header = integer_to_object(size);
    gc->current_index += size + 1;
    return v;
}
/* The size field is used to store redirections during GC. */
static inline object vector_moved(vector *v) {
    if (object_to_vector(v->header)) return v->header;
    else return 0;
}


/* Move an object.
   - atom: copy reference
   - vector: - moved? simply return forwarding pointer
             - old space? move it and return new address. */


static object _gc_move_object(gc *gc, object o_old) {
    object o_new;
    vector *v_old = object_to_vector(o_old);

    /* Keep atom pointers. */
    if (!v_old) return o_old;

    /* Already moved -> copy forwarding pointer. */
    if (!(o_new = vector_moved(v_old))) {

        /* Copy object */
        long size = vector_size(v_old);
        long bytes = sizeof(long) * size;
        vector *v_new = _gc_allot(gc, size);
        o_new = vector_to_object(v_new);
        memcpy(v_new->slot, v_old->slot, bytes); // copy contents
        memset(v_old->slot, 0, bytes);           // remove finalizers
        v_new->header = v_old->header;           // preserve tags
        v_old->header = o_new;                   // forward old
    }
    return o_new;
}
#if GC_CHENEY
static object gc_mark(gc *gc, object root) {
    long todo = gc->current_index;
    /* Start with moving the root. */
    root = _gc_move_object(gc, root);
    /* Now move all objects referenced in moved objects. */
    while(todo < gc->current_index) {
        vector *v = &gc->current[todo];
        long size = vector_size(v);
        long i;
        for (i=0; i<size; i++) {
            object new = _gc_move_object(gc, v->slot[i]);
            v->slot[i] = new;
        }
        todo += size + 1;
    }
    return root;
}
#else
#warning Using recursive GC (GC_CHENEY=0)
static object gc_mark(gc *gc, object o_old) {

    /* Can only mark vectors.  Other objects are copied. */
    vector *v_old = object_to_vector(o_old); 
    if (!v_old) return o_old;

    /* Check if it's already marked + moved */
    object object_moved = vector_moved(v_old);
    if (object_moved) return object_moved;

    /* Allocate empty vector. */
    long nb = vector_size(v_old);
    vector *v_new = gc_alloc(gc, nb);
    object o_new =  vector_to_object(v_new);

    /* Copy the tag bits. */
    v_new->header = v_old->header;

    /* Mark the old header as moved before recursing. */
    v_old->header = o_new;

    /* Mark all and move elements and erase tracks. */
    long i;
    for (i=0; i<nb; i++) {
        v_new->slot[i] = gc_mark(gc, v_old->slot[i]);
        v_old->slot[i] = 0;
    }
    return o_new;                        
}
#endif

static void _gc_when_full(gc *gc, long slots) {
    /* Record request size that triggered GC to check if it will fit
       _after_ collection. */
    gc->want = slots;
    gc_collect(gc);
}
static vector *gc_alloc(gc *gc, long size) {
    long slots = size + 1;
    if (unlikely(gc_full(gc, slots))) {
        _gc_when_full(gc, slots);
    }
    return _gc_allot(gc, size);
}
static void _gc_call_finalizers(gc *gc) {
    _gc_fin_slots(gc, (object *)gc->old, gc->old_index);
    gc->old_index = 0;
}
static void _gc_finalize(gc *gc) {
    _gc_call_finalizers(gc);
    /* If we need to grow, send a message to the client. */
    long nb_extra = gc->want - (gc->slot_total - gc->current_index);
    if (nb_extra > 0) gc->overflow(gc->client_ctx, nb_extra);
}
static void _gc_swap(gc *gc) {
    vector *current   = gc->current;
    vector *old       = gc->old;
    gc->old           = current;
    gc->old_index     = gc->current_index;
    gc->current       = old;
    gc->current_index = 0;
}

static void _gc_collect_with_fin(gc *gc, gc_finalize fin) {

    /* Won't re-enter. */
    gc_assert(!gc->old_index);

    /* After this gc_alloc() will take from the new space. */
    _gc_swap(gc);

    /* Call client to mark the root pointers and pass the continuation
       so client can abort the C stack when a collection is triggered.
       If this passes fin == NULL, the client needs to return. */
    gc->mark_roots(gc->client_ctx, fin);
}

static void gc_collect(gc *gc) { 
    _gc_collect_with_fin(gc, _gc_finalize); 
}
static void _gc_collect_no_abort(gc *gc) {
    /* Client is not allowed to abort if we pass NULL finalizer. */
    _gc_collect_with_fin(gc, NULL);
    _gc_call_finalizers(gc);
}

static int gc_grow(gc *gc, long add_slots) {
    /* grow pool */
    long total = gc->slot_total;
    total += add_slots; // make sure there will be enough
    long bytes = total * sizeof(object);
    
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    _gc_collect_no_abort(gc);
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    gc->slot_total = total;
    return 1;
}

static gc *gc_new(long total, void *ctx, gc_mark_roots mark, 
                  gc_overflow  overflow) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total     = total;
    x->current        = (vector*)calloc(total, sizeof(object));
    x->old            = (vector*)calloc(total, sizeof(object));
    x->current_index  = 0;
    x->old_index      = 0;
    x->want           = 0;
    x->mark_roots     = mark;
    x->overflow       = overflow;
    x->client_ctx     = ctx;
    return x;
}




#endif
