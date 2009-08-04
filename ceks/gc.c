#include <stdlib.h>
#include <stdio.h>

/* Simple copying GC for allocating vectors of pointers.

   The GC uses the following annotations:

   1. how many elements a vector contains

      The first element of a struct vector_ contains the size, along
      side some bits that can be used for tagging by the application.

   2. whether a pointer is to another vector or an atom

      LSB=0 Foreign pointer
      LSB=1 Managed pointer 

   3. how to free() an atom

      An atom is a struct where the first element is a free() method.

   4. wheter a vector has moved to the other buffer

      SIZE LSB=0  Active vector with object count
      SIZE LSB=1  Reference to vector in other buffer

*/


typedef struct _atom atom;
typedef void (*atom_free)(atom *);
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
    v->size = size << 2;
}
static long vector_size(vector *v) {
    return v->size >> 2;
}
static vector *object_vector(object ob) {
    if(ob & 1) return ((vector*)(ob&(-3)));
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
    if (ob & 1) return NULL;
    else return (atom*)ob;
}






void gc_collect(gc *gc);

static vector *gc_alloc_vector(gc *gc, long size) {
    int attempts = 1;
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
            gc->slot_index = slot_index_new;
            return v;
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

object gc_mark_and_move(gc *gc, object o_old) {
    object o_new;
    vector *v_old = object_vector(o_old);
    GC_ASSERT(v_old); // can't mark atoms

    long i,nb = vector_size(v_old); // size field used for

    vector *v_new = gc_alloc_vector(gc, nb);
    o_new = vector_to_object(v_new);
    vector_set_moved(v_old, o_new); // patch through to new location

    for (i=0; i<nb; nb++) {
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
            v_new->el[i] = gc_mark_and_move(gc, obj);
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
            a->free(a);
        }
    }
}

gc *gc_new(long total) {
    gc *gc = (gc*)malloc(sizeof(*gc));
    gc->slot_total = total;
    gc->current    = (object*)malloc(sizeof(object) * total);
    gc->old        = (object*)malloc(sizeof(object) * total);
    gc->slot_index = 0;
    gc->roots      = gc_alloc_vector(gc, 1);
}

#ifdef TEST
void print_gc(gc *gc) {
    int level;
}

void debug(void) {
    gc *gc = gc_new(100);
}
int main(int argc, char **argv) {
    for(;;) debug();
}
#endif
