#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

/* Simple copying GC for allocating graphs of vectors and atoms.

   The GC uses the following annotations:

   1. VECTOR-SIZE

      For a live object, the first element of a struct vector_
      contains the size.

   2. VECTORS?

      Cells are tagged using 2 tag bits.

   3. ATOM-FREE

      An atom is a struct where the first element is a free() method.

   4. MOVED?

      A moved object contains an an object reference in the size slot.

*/

/* LSbit TYPE TAGS:

00 atom pointing to struct with free() method
01 vector
10 integer
11 not used

*/

#include "gc.h"
#include "gc_config.h"





int gc_grow(gc *gc, long size) {
    /* grow pool */
    long total = gc->slot_total;
    total += size;      // make sure there will be enough
    total += (total/4); // and add a bit more
    long bytes = total * sizeof(object);
    
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    gc_collect(gc);
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    gc->slot_total = total;
    return 1;
}


static inline int gc_full(gc *gc, int slots) {
    return (gc->slot_index + slots) >= gc->slot_total;
}

/* Allocate a vector. */
object gc_alloc(gc *gc, long size) {
    long slots = size + 1;
    if (gc_full(gc, slots)) {
        gc_collect(gc);
        if (gc_full(gc, slots)) {
            if (!gc_grow(gc, slots)) return 0;
        }
    }
    vector *v = (vector *)(&gc->current[gc->slot_index]);
    v->tag_size = integer_to_object(size);
    gc->slot_index += slots;
    return vector_to_object(v);
}

/* The size field is used to store redirections during GC. */
static inline object vector_moved(vector *v) {
    if (object_to_vector(v->tag_size)) return v->tag_size;
    else return 0;
}
static inline void vector_set_moved(vector *v, object o) {
    v->tag_size = o;
}

object gc_mark(gc *gc, object o_old) {

    /* Allocate empty vector. */
    vector *v_old = object_to_vector(o_old); 
    long nb = object_to_vector_size(v_old->tag_size);
    object o_new = gc_alloc(gc, nb);
    vector *v_new = object_to_vector(o_new);

    /* Mark the old header as moved before recursing. */
    vector_set_moved(v_old, o_new); 

    long i;
    for (i=0; i<nb; i++) {
        object obj = v_old->slot[i];
        object obj_moved;
        vector *v;

        if (!(v = object_to_vector(obj))) {
            v_new->slot[i] = obj; // copy ref
            v_old->slot[i] = 0;   // erase ref => no free()
        }
        else if ((obj_moved = vector_moved(v))) {
            v_new->slot[i] = obj_moved;
        }
        else {
            v_new->slot[i] = gc_mark(gc, obj);
        }
    }
    return o_new;                        
}

void gc_collect(gc *gc) {

    /* Record the current used size and swap buffers.  After this
       gc_alloc_vector() will take from the new space. */
    object *current = gc->old;
    object *old     = gc->current;
    int had         = gc->slot_index;

    gc->current     = current;
    gc->old         = old;
    gc->slot_index  = 0;

    /* Recursively mark + copy */
    gc->roots = gc_mark(gc, gc->roots);

    /* Free all atoms */
    long i;
    for (i=0; i<had; i++){
        atom *a;
        if ((a = object_to_atom(gc->old[i]))) {
            if (a->op) {
                a->op->free(a);
            }
        }
        gc->old[i] = 0;
    }
    if (gc->notify_fn)  {
        gc->notify_fn(gc->notify_ctx);
    }
}

gc *gc_new(long total, gc_notify fn, void *ctx) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total = total;
    x->current    = (object*)malloc(total * sizeof(object));
    x->old        = (object*)malloc(total * sizeof(object));
    x->slot_index = 0;
    x->roots      = gc_alloc(x, 1);
    x->notify_fn  = fn;
    x->notify_ctx = ctx;
    return x;
}





object gc_vector(gc *gc, long slots, ...) {
    va_list ap;
    object o = gc_alloc(gc, slots);
    vector *v = object_to_vector(o);
    long i = 0;
    va_start(ap, slots);
    for (i=0; i<slots; i++) {
        v->slot[i] = va_arg(ap, object);
    }
    va_end(ap);
    return o;
}



