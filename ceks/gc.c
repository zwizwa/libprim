#include <stdlib.h>
#include <stdio.h>

/* Simple copying GC for allocating graphs of vectors and atoms.

   The GC uses the following annotations:

   1. how many elements a vector contains

      The first element of a struct vector_ contains the size, along
      side some bits that can be used for tagging by the application.

   2. whether a pointer is to another vector or an atom

   3. how to free() an atom

      An atom is a struct where the first element is a free() method.

   4. wheter a vector has moved to the other buffer

      In this case the size field is replaced by a 01 pointer.

*/

/* LSbit TYPE TAGS:

00 atom pointing to struct with free() method
01 vector
10 integer
11 not used

*/


typedef struct _atom atom;
typedef void (*atom_free)(void *);
typedef struct _vector vector;
typedef long object;

struct _atom {
    atom_free free;
};

struct _vector {
    long size;
    object el[0];
};

typedef struct {
    object *current;
    object *old;

    long slot_index;
    long slot_total;

    object roots;

} gc;

// private data structure access
static void vector_set_size(vector *v, long size) {
    v->size = 2 + (size << 2);  // tagged int
}
static long vector_size(vector *v) {
    return v->size >> 2;
}
static vector *object_vector(object ob) {
    if(ob & 1) { ob &= 0xFFFFFFFFFFFFFFFCL; return ((vector*)ob); }
    else return NULL;
}
static object vector_to_object(vector *v) {
    return ((object)v) + 1;
}


// public access
long object_vec_size(object obj) {
    vector *v = object_vector(obj);
    if (!v) return -1; // not a vector
    return vector_size(v);
}
object object_vec_ref(object obj, long offset) {
    vector *v = object_vector(obj);
    return v->el[offset];
}
object object_vec_set(object obj, long offset, object x) {
    vector *v = object_vector(obj);
    v->el[offset] = x;
}
atom *object_atom(object ob) {
    if (ob & 3) return NULL;
    else return (atom*)ob;
}


void gc_collect(gc *gc);

object gc_alloc(gc *gc, long size) {
    int attempts = 2;
    long slots, slot_index_new;
    slots = size + 1;
    while(attempts){
        slot_index_new = gc->slot_index + slots;
        if (slot_index_new >= gc->slot_total) {
            gc_collect(gc);
            attempts--;
        }
        else {
            vector *v = (vector *)(&gc->current[gc->slot_index]);
            vector_set_size(v, size);
            gc->slot_index = slot_index_new;
            return vector_to_object(v);
        }
    }
    /* grow pool */
    printf("out of memory\n");
    exit(1);
}

/* The size field */
static inline object vector_moved(vector *v) {
    if (object_vector(v->size)) return v->size;
    else return 0;
}
static inline void vector_set_moved(vector *v, object o) {
    v->size = o;
}

#define GC_ASSERT(x) if(!x) exit(1);

object gc_mark_and_copy(gc *gc, object o_old) {
    vector *v_old = object_vector(o_old);
    GC_ASSERT(v_old); // can't mark atoms

    long i,nb = vector_size(v_old); // size field used for

    object o_new = gc_alloc(gc, nb);
    vector *v_new = object_vector(o_new);

    vector_set_moved(v_old, o_new); // patch through to new location

    for (i=0; i<nb; i++) {
        object obj = v_old->el[i];
        object obj_moved;
        vector *v;

        if (!(v = object_vector(obj))) {
            v_new->el[i] = obj; // copy ref
            v_old->el[i] = 0;   // erase ref => no free()
        }
        else if ((obj_moved = vector_moved(v))) {
            v_new->el[i] = obj_moved;
        }
        else {
            v_new->el[i] = gc_mark_and_copy(gc, obj);
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
    gc->roots = gc_mark_and_copy(gc, gc->roots);

    /* Free all atoms */
    long i;
    for (i=0; i<had; i++){
        atom *a;
        if ((a = object_atom(gc->old[i]))) {
            printf("free(%p)\n", a);
            a->free(a);
        }
        gc->old[i] = 0;
    }
}



gc *gc_new(long total) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total = total;
    x->current    = (object*)calloc(total, sizeof(object));
    x->old        = (object*)calloc(total, sizeof(object));
    x->slot_index = 0;
    x->roots      = gc_alloc(x, 1);
    return x;
}

#ifdef TEST
void debug(gc *gc) {
    atom *a = malloc(sizeof(*a));
    a->free = free;
    object_vec_set(gc_alloc(gc, 1), 0, (object)a);
    object_vec_set(gc->roots, 0, gc_alloc(gc, 1));
    // gc_collect(gc);
}
int main(int argc, char **argv) {
    gc *gc = gc_new(20);
    for(;;) debug(gc);
}
#endif
