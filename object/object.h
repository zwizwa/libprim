#ifndef _OBJECT_H_
#define _OBJECT_H_

#include "port.h"   /* For writing. */

/* Tagged objects.  Used in nonlinear and linear GC. */

/* The size field in the _vector struct can be used to store
   additional tag bits.  GC_TAG_MASK tells the GC what to ignore. */


/* Vectors can have tags in the upper bits.  Currently 5 bits are
   reserved, and the first couple are shared by the Scheme and PF data
   models. */
#define GC_VECTOR_TAG_BITS 5
#ifdef _LP64
#define GC_VECTOR_TAG_SHIFT (64 - GC_VECTOR_TAG_BITS)
#define GC_VECTOR_TAG_MASK ((1L<<GC_VECTOR_TAG_SHIFT)-1L)
#else
#define GC_VECTOR_TAG_SHIFT (32 - GC_VECTOR_TAG_BITS)
#define GC_VECTOR_TAG_MASK ((1<<GC_VECTOR_TAG_SHIFT)-1)
#endif
static inline unsigned long VECTOR_TAG(unsigned long x) {
    return x << GC_VECTOR_TAG_SHIFT;
}
#define TAG_VECTOR    VECTOR_TAG(0)   /* The flat vector. */
#define TAG_PAIR      VECTOR_TAG(1)   /* The CONS cell. */
#define TAG_AREF      VECTOR_TAG(2)   /* Reference with finalization. */
#define TAG_OPAQUE    VECTOR_TAG(3)   /* An "outside" pointer. */

#define GC_VECTOR_USER_START 8



/* Base objects have a 2-bit tag. */
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

#define unlikely(x) __builtin_expect((x),0)

typedef unsigned long object;
typedef long integer;
typedef void (*fin)(void *);

typedef struct _vector vector;

struct _vector {
    unsigned long header;  // [ struct_tags | length | GC tags ]
    object slot[0];
};

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


/* Primitive structs used in sc are identifiable by the pointer they
   contain as a first member.  These are represented as GC_CONST.  The
   addresses 0x000->0xFFF (first 4K page) are reserved for
   constants. */

#define GC_CONST_MASK 0xFFF

/* Booleans and void are encoded as constant pointers. */
#define NIL   ((object)0)
#define CONSTANT(x) const_to_object((void*)((((x)<<1)|1)<<GC_TAG_SHIFT))
#define FALSE CONSTANT(0)
#define TRUE  CONSTANT(1)
#define VOID  CONSTANT(2)

static inline void *object_struct(object ob, void *type){
    void *x = object_to_const(ob);
    if ((((long)x) & (~GC_CONST_MASK)) == 0) return NULL; // constant
    if (type != *((void**)x)) return NULL;
    return x;
}

static inline unsigned long vector_to_flags(vector *v) {
    return (v->header) & (~GC_VECTOR_TAG_MASK);
}
static inline void vector_set_flags(vector* v, long flags){
    v->header |= flags;
}
static inline unsigned long object_get_vector_flags(object o){
    vector *v = object_to_vector(o);
    if (!v) return -1;
    return vector_to_flags(v);
}


#define DEF_CAST(type)                                \
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_vector(o); }




#endif
