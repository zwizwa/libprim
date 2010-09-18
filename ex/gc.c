#include "config.h"
#include "gc.h"

/* PRIVATE + implementation */


/* Basic allocation functions are inline. */
static inline int gc_full(gc *gc, int slots) {
    return (gc->current_index + slots) >= gc->slot_total;
}
/* User must fill the allocated space with valid tagged values before
   calling gc_alloc again. */
static void _gc_when_full(gc *gc, long size);
#if 0
static inline _ gc_aref(gc *gc, void *fn, _ ob) {
    vector *v = gc_alloc(gc, 2);
    vector_set_flags(v, TAG_AREF);
    v->slot[0] = fin_to_object(fn);
    v->slot[1] = ob;
    return vector_to_object(v);
}

static inline _ gc_box(gc *gc, object init) {
    vector *v = gc_alloc(gc, 1);
    vector_set_flags(v, TAG_BOX);
    v->slot[0] = init;
    return vector_to_object(v);
}
#endif
object gc_make_tagged_v(gc *gc, long tag, long slots, va_list ap) {
    vector *v = gc_alloc(gc, slots);
    long i = 0;
    for (i=0; i<slots; i++) {
        v->slot[i] = va_arg(ap, object);
    }
    vector_set_flags(v, tag);
    return vector_to_object(v);
}
object gc_make_tagged(gc *gc, long tag, long slots, ...) {
    va_list ap;
    va_start(ap, slots);
    object o = gc_make_tagged_v(gc, tag, slots, ap);
    va_end(ap);   
    return o;
}
void _gc_assert(const char *cond, const char *file, int line) {
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
            (*f)(o[i+1], gc->client_ctx);
            i++;
        }
    }
}

static vector *_gc_allot(gc *gc, long size) {
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
object gc_mark(gc *gc, object root) {
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
vector *gc_alloc(gc *gc, long size) {
//    if (size > GC_MAX_VECTOR_SIZE) return NULL;

    if (unlikely(NULL == gc)) {
        fprintf(stderr, "ERROR: GC: Allocation disabled.\n");
        kill(getpid(), SIGTRAP);
    }
    // fprintf(stderr, ".");
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
    long free =  (gc->slot_total - gc->current_index);
    long margin = gc->margin + gc->current_index;  // keep free space +- same size as used
    long spare = free - (margin + gc->want);
    if (spare < 0) gc->overflow(gc->client_ctx, -spare);
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

void gc_collect(gc *gc) { 
    _gc_collect_with_fin(gc, _gc_finalize); 
}
static void _gc_collect_no_abort(gc *gc) {
    /* Client is not allowed to abort if we pass NULL finalizer. */
    _gc_collect_with_fin(gc, NULL);
    _gc_call_finalizers(gc);
}

int gc_grow(gc *gc, long add_slots) {
    /* grow pool */
    long total = gc->slot_total;
    total += add_slots; // make sure there will be enough
    long bytes = total * sizeof(object);
    
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    memset(gc->old, 0, bytes);
    _gc_collect_no_abort(gc);
    if (!(gc->old = realloc(gc->old, bytes))) return 0;
    memset(gc->old, 0, bytes);
    gc->slot_total = total;
    return 1;
}

gc *gc_new(long total, void *ctx, gc_mark_roots mark, 
           gc_overflow  overflow) {
    gc* x = (gc*)calloc(1, sizeof(gc));
    x->slot_total     = total;
    x->current        = (vector*)calloc(total, sizeof(object));
    x->old            = (vector*)calloc(total, sizeof(object));
    x->current_index  = 0;
    x->old_index      = 0;
    x->want           = 0;
    x->mark_roots     = mark;
    x->overflow       = overflow;
    x->client_ctx     = ctx;


    /* Never leave less then this number of cells available after an
       alloction that triggers a collection.  This is to avoid restart
       loops.  The number needs to be larger than the maximal amount
       of cells necessary to complete a single interpretation step. */
    x->margin         = 100;
    return x;
}
