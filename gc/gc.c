#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>

/* Simple stop-and-copy GC for allocating graphs of vectors and atoms.

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



#include "gc_config.h"

void gc_when_full(gc *gc, long slots) {
    gc_collect(gc);
    if (gc_full(gc, slots)) {
        if (!gc_grow(gc, slots)) {
            fprintf(stderr, "ERROR: Can't grow GC pool.\n");
        }
    }
}

int gc_grow(gc *gc, long add_slots) {
    /* grow pool */
    long total = gc->slot_total;
    total += add_slots; // make sure there will be enough
    total += (total/4); // and add a bit more
    long bytes = total * sizeof(object);
    
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    gc_collect(gc);
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    gc->slot_total = total;
    return 1;
}


/* The size field is used to store redirections during GC. */
static inline object vector_moved(vector *v) {
    if (object_to_vector(v->header)) return v->header;
    else return 0;
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


void gc_fin_slots(gc *gc, object *o, long slots) {
    long i;
    for (i=0; i<slots; i++){
        fin *f;
        if ((f = object_to_fin(o[i]))) {
            o[i] = 0;
            (*f)(object_to_const(o[i+1]));
            i++;
        }
    }
}

vector *gc_alloc(gc *gc, long size) {
    long slots = size + 1;
    if (unlikely(gc_full(gc, slots))) {
        gc_when_full(gc, slots);
    }
    // finalize data before overwriting
    gc_fin_slots(gc, &gc->current[gc->current_index], slots);

    vector *v = (vector *)(&gc->current[gc->current_index]);
    v->header = integer_to_object(size);
    gc->current_index += slots;
    return v;
}


static void _finalize(gc *gc) {
    // gc_fin_slots(gc, gc->old, gc->old_index);
    gc->old_index = 0;
}

static void _swap(gc *gc) {

    object *current   = gc->current;
    object *old       = gc->old;

    gc->old           = current;
    gc->old_index     = gc->current_index;
    gc->current       = old;
    gc->current_index = 0;
}

void gc_collect(gc *gc) {

    if (gc->old_index) {
        /* If gc_collect() can't be called by a gc_alloc() triggered
           by a gc_collect() because the data did fit before. */
        fprintf(stderr, "ERROR: re-entering gc_collect(): "
                "corrupt heap.\n");
        kill(getpid(), SIGTRAP);
    }

    /* Record the current used size and swap buffers.  After this
       gc_alloc() will take from the new space. */
    _swap(gc);

    /* Call client to mark the root pointers and pass the continuation
       so client can abort the C stack when a collection is
       triggered. */
    gc->mark_roots(gc->mark_roots_ctx, _finalize);
}

gc *gc_new(long total, gc_mark_roots fn, void *ctx) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total     = total;
    x->current        = (object*)calloc(total, sizeof(object));
    x->old            = (object*)calloc(total, sizeof(object));
    x->current_index  = 0;
    x->old_index      = 0;
    x->mark_roots     = fn;
    x->mark_roots_ctx = ctx;
    return x;
}


