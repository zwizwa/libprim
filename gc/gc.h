#ifndef _GC_H_
#define _GC_H_

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


/* GC is implemented in a header file, since its code depends on
   configuration that changes the binary heap format (vector tags). */


#define unlikely(x) __builtin_expect((x),0)

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>


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
    vector  *current;
    long    current_index;
    vector  *old;
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




static gc *gc_new(long total, gc_mark_roots fn, void *ctx);
static void gc_collect(gc *gc);
static object gc_mark(gc *gc, object o_old);
#define GC_CHENEY 1



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
static int gc_grow(gc *gc, long add_slots);
static inline int gc_full(gc *gc, int slots) {
    return (gc->current_index + slots) >= gc->slot_total;
}
/* User must fill the allocated space with valid tagged values before
   calling gc_alloc again. */
static void gc_when_full(gc *gc, long size);
static vector *gc_alloc(gc *gc, long size);

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


static void gc_do_assert(const char*, const char*, int);
// __FUNCTION__
#define gc_assert(x) {if (!(x)) gc_do_assert(#x, __FILE__, __LINE__);}




static void gc_when_full(gc *gc, long slots) {
    gc_collect(gc);
    if (gc_full(gc, slots)) {
        if (!gc_grow(gc, slots)) {
            fprintf(stderr, "ERROR: Can't grow GC pool.\n");
        }
    }
}

static int gc_grow(gc *gc, long add_slots) {
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


static void gc_fin_slots(gc *gc, object *o, long slots) {
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


/* The recursive mark algorithm traverses objects depth-first from a
   root object, using the C-stack for traversal state.  The Cheney
   algorithm uses the tospace as a queue to sequence a breadth-first
   search. */

static void gc_assert_old(gc *gc, object o) {
    vector *v = object_to_vector(o);
    gc_assert((!v) ||
              ((v >= gc->old) &&
               (v <  gc->old + gc->old_index)));
}
static void gc_assert_current(gc *gc, object o) {
    vector *v = object_to_vector(o);
    gc_assert((!v) ||
              ((v >= gc->current) &&
               (v <  gc->current + gc->current_index)));
}


static vector *_gc_allot(gc *gc, long size) {
    vector *v = &gc->current[gc->current_index];
    // finalize data before overwriting
    // gc_fin_slots(gc, &v->header, size + 1);
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
    /* Input objects need to be in the old space. */
    /**/ gc_assert_old(gc, o_old);

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
    /* Output vectors need to be in new space. */
    /**/ gc_assert_current(gc, o_new);
    return o_new;
}



static void gc_do_assert(const char *cond, const char *file, int line) {
    fprintf(stderr, "%s: %d: gc_assert(%s)\n", file, line, cond);
    kill(getpid(), SIGTRAP);
    exit(1);
}

static void gc_assert_all_current(gc *gc) {
    long i;
    object *o = (object*)gc->current;
    for (i=0; i<gc->current_index; i++) {
        gc_assert_current(gc, o[i]);
    }
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
    /**/ gc_assert_all_current(gc);
    printf("%ld\n", todo);
    return root;
}
#else
#warning Using recursive GC.
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

static vector *gc_alloc(gc *gc, long size) {
    long slots = size + 1;
    if (unlikely(gc_full(gc, slots))) {
        gc_when_full(gc, slots);
    }
    // get new
    return _gc_allot(gc, size);
}


static void _finalize(gc *gc) {
    gc_fin_slots(gc, (object *)gc->old, gc->old_index);
    gc->old_index = 0;

    memset(gc->old, 0, sizeof(long) * gc->slot_total);

}

static void _swap(gc *gc) {

    vector *current   = gc->current;
    vector *old       = gc->old;

    gc->old           = current;
    gc->old_index     = gc->current_index;
    gc->current       = old;
    gc->current_index = 0;
}

static void gc_collect(gc *gc) {

    /* gc_collect() can't be called by a gc_alloc() triggered by a
       gc_collect() because the data did fit before. */
    gc_assert(!gc->old_index);

    /* Record the current used size and swap buffers.  After this
       gc_alloc() will take from the new space. */
    _swap(gc);

    /* Call client to mark the root pointers and pass the continuation
       so client can abort the C stack when a collection is
       triggered. */
    gc->mark_roots(gc->mark_roots_ctx, _finalize);
}

static gc *gc_new(long total, gc_mark_roots fn, void *ctx) {
    gc* x = (gc*)malloc(sizeof(gc));
    x->slot_total     = total;
    x->current        = (vector*)calloc(total, sizeof(object));
    x->old            = (vector*)calloc(total, sizeof(object));
    x->current_index  = 0;
    x->old_index      = 0;
    x->mark_roots     = fn;
    x->mark_roots_ctx = ctx;
    return x;
}




#endif
