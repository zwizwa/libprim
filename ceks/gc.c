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
    return (gc->current_index + slots) >= gc->slot_total;
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
    vector *v = (vector *)(&gc->current[gc->current_index]);
    v->tag_size = integer_to_object(size);
    gc->current_index += slots;
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

    /* Can only mark vectors.  Other objects are copied. */
    vector *v_old = object_to_vector(o_old); 
    if (!v_old) return o_old;

    /* Check if it's already marked + moved */
    object object_moved = vector_moved(v_old);
    if (object_moved) return object_moved;

    /* Allocate empty vector. */
    long nb = vector_size(v_old);
    object o_new = gc_alloc(gc, nb);
    vector *v_new = object_to_vector(o_new);

    /* Mark the old header as moved before recursing. */
    vector_set_moved(v_old, o_new);

    /* Mark all and move elements and erase tracks. */
    long i;
    for (i=0; i<nb; i++) {
        v_new->slot[i] = gc_mark(gc, v_old->slot[i]);
        v_old->slot[i] = 0;
    }
    return o_new;                        
}

void gc_collect(gc *gc) {

    /* Record the current used size and swap buffers.  After this
       gc_alloc() will take from the new space. */
    object *current   = gc->current;
    object *old       = gc->old;

    gc->old           = current;
    gc->old_index     = gc->current_index;
    gc->current       = old;
    gc->current_index = 0;

    /* Call client to mark the root pointers. */
    gc->mark_roots(gc->mark_roots_ctx);

    /* Free all atoms */
    long i;
    for (i=0; i<gc->old_index; i++){
        atom *a;
        if ((a = object_to_atom(gc->old[i]))) {
            if (a->op) {
                a->op->free(a);
            }
        }
        gc->old[i] = 0;
    }
}

/* If the GC is called during the execution of a primitive, the
   recently allocated vectors might not have made it into the roots
   yet.  Therefore the GC can be marked before the interpreter
   mainloop starts, such that in the event of an allocation, these
   newer objects can be bundled and added to the root.
*/
object gc_mark_wild(gc *gc) {
    gc->wild = gc->current_index;
}

/* This function needs to run before any vector is marked and moved
   (to make sure we can navigate the heap using the length slots not
   yet replaced with indirections), but after the allocation buffers
   are swapped so gw_alloc() will work. */

object gc_border_to_vector(gc *gc, long index) {
    long nb = 0;
    long i = index;
    // traverse once to find the size
    for(i=index; i<gc->old_index;) {
        long size = object_to_vector_size(gc->old[i]);
        i += size+1; nb++;
    }
    if (!nb) return integer_to_object(0); // don't alloc
    // allocate vector
    object vo = gc_alloc(gc, nb);
    vector *v = object_to_vector(vo);
    long j;
    // traverse again to get the pointers
    for(j=0, i=index; i<gc->old_index;) {
        long size = object_to_vector_size(gc->old[i]);
        v->slot[j] = vector_to_object((vector*)(&gc->old[i]));
        i += size+1; j++;
    }
    // mark all pointers
    for(j=0, j<nb, j++) {
        v->slot[j] = gc_mark(gc, v->slot[j]);
    }
    return vo;
}

gc *gc_new(long total, gc_mark_roots fn, void *ctx) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total = total;
    x->current    = (object*)malloc(total * sizeof(object));
    x->old        = (object*)malloc(total * sizeof(object));
    x->slot_index = 0;
    x->root       = gc_alloc(x, 1);
    x->mark_roots = fn;
    x->mark_roots_ctx = ctx;
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



