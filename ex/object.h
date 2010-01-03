#ifndef _OBJECT_H_
#define _OBJECT_H_

#include <leaf/port.h>  /* For writing. */

/* Objects with type tags.  Used in nonlinear and linear GC.  These
   can reference integer, opaque leaf objects (and finalizers) or
   transparent vector objects.

   Note there are 4 kinds of type tagging:

      - LSB of object, distinguishing 4 basic types for GC.
      - MSB of vector.header to represent high level types (DEF_STRUCT)
      - a GC_CONST 0:FFF is a numeric constant (i.e. TRUE,FALSE,NIL,VOID)
      - first pointer field of GC_CONST to identify opaque types (DEF_ATOM)
 */

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


/* The vector is the basis of the (transparent) object
   representation. */
typedef unsigned long object;   
typedef object _; // this alias reduces noise a lot.
typedef long integer;
typedef void (*fin)(object, void *gc_ctx);
typedef struct _vector vector;
struct _vector {
    unsigned long header;  // [ tags | length | GC tags ]
    object slot[0];
};

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
#define GC_MAX_VECTOR_SIZE ((1<<(GC_VECTOR_TAG_SHIFT - GC_TAG_SHIFT))-1)
static inline unsigned long VECTOR_TAG(unsigned long x) {
    return x << GC_VECTOR_TAG_SHIFT;
}

/* To make things easier to manage, all vectors type tags for SC and
   PF are defined centrally. */

#define TAG_VECTOR    VECTOR_TAG(0)   /* The flat vector. */
#define TAG_AREF      VECTOR_TAG(1)   /* Reference with finalization. */
#define TAG_ERROR     VECTOR_TAG(2)

/* Interpreter data types. */
#define TAG_REDEX     VECTOR_TAG(4)
#define TAG_VALUE     VECTOR_TAG(5)
#define TAG_STATE     VECTOR_TAG(6)
#define TAG_LAMBDA    VECTOR_TAG(7)

/* List polymorphy is handled using tag ranges.

   List structures (read-only polymorphy): 8-15 
   Linear list structures: 12-15 */

#define TAG_PAIR      VECTOR_TAG(8)   /* The CONS cell. */

#define TAG_LPAIR     VECTOR_TAG(12)  /* Linear data list pair. */
#define TAG_LNEXT     VECTOR_TAG(13)  /* Linear code continuation frame. */
#define TAG_LDATA     VECTOR_TAG(14)  /* Linear data continuation frame. */

// note, these need to be powers of 2!
#define VECTOR_TAG_IN_RANGE(flag, start, nb) \
    ((flag & VECTOR_TAG(~(nb-1))) == VECTOR_TAG(start))

static inline long flags_is_poly_pair(long flag) {
    return VECTOR_TAG_IN_RANGE(flag, 8, 8);
}
static inline long flags_is_linear_pair(long flag) {
    return VECTOR_TAG_IN_RANGE(flag, 12, 4);
}

/* PF code tags. */
#define TAG_LIN   VECTOR_TAG(16)
#define TAG_BOX   VECTOR_TAG(17)
#define TAG_QUOTE VECTOR_TAG(18)
#define TAG_SEQ   VECTOR_TAG(19)


/* Scheme continuation tags: 24-31 */
#define TAG_K_IF      VECTOR_TAG(24)
#define TAG_K_SET     VECTOR_TAG(25)
#define TAG_K_ARGS    VECTOR_TAG(26)
#define TAG_K_SEQ     VECTOR_TAG(27)

static inline long flags_is_k(long flag) {
    return VECTOR_TAG_IN_RANGE(flag, 24, 8);
}



/* Atoms that need finalization must be wrapped to ensure that they
   occur only once in the heap: the finalize() method is called for
   each garbage copy that's encountered. */
typedef struct {
    vector v;
    _ fin;
    _ object;
} aref;

typedef struct {
    vector v;
    object car;
    object cdr;
} pair;

typedef pair lpair;  // linear pair
typedef pair lnext;  // PF's NEXT K-FRAME
typedef pair ldata;  //  ..  DATA


/* Conversion from tagged objects to one of the 4 C data types.  When
   the tag doesn't match, NULL is returned.  Conversion to integer
   always succeeds. (FIXME: make this word index size, i.e. on 64 bits
   use GC_TAG_SHIFT = 3)
*/

static inline vector *object_to_vector(object ob) {
    if(GC_VECTOR == GC_TAG(ob)) return (vector*)GC_POINTER(ob);
    else return NULL;
}


#define unlikely(x) __builtin_expect((long)(x),0)
#define likely(x) __builtin_expect((long)(x),1)


/* Note: be careful: this is a _pointer_ to fin.  Code isn't always
   aligned properly to accomodate GC tag bits, but data alignment can
   be enforced (i.e. malloc() returns aligned data).  Embedding the
   code pointer in a data structure seems to be the right approach. */
static inline fin *object_to_fin(object ob) {
    if (GC_FIN == GC_TAG(ob)) return (fin*)GC_POINTER(ob);
    else return NULL;
}
static inline void *object_to_const(object ob) {
    if (GC_CONST == GC_TAG(ob)) return (void*)GC_POINTER(ob);
    else return NULL;
}
static inline long object_to_integer(object o) { 
    long i = (long)o;
    return (i >> GC_TAG_SHIFT);
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
#define EOF_OBJECT CONSTANT(3)




/* If the object is a leaf object, this will return a non-null pointer. */
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
// note: vector_set_flags only performs bitwise OR
static inline void vector_reset_flags(vector *v, long flags) {
    v->header &= GC_VECTOR_TAG_MASK;
    v->header |= flags;
}

static inline unsigned long object_get_vector_flags(object o){
    vector *v = object_to_vector(o);
    if (!v) return -1;
    return vector_to_flags(v);
}

/* Conversion from object to a transparent struct (tagged vector). */
static inline void *object_to_struct(object ob, long tag) {
    vector *v = object_to_vector(ob);
    if (!v) return NULL;
    if (tag == vector_to_flags(v)) return (void*)v;
    else return NULL;
}
#define DEF_STRUCT(type, tag)                                  \
    static inline type *object_to_##type(object o) {       \
        return (type*)object_to_struct(o, tag); }

/* Unsafe list macros */
#define _CAR(o)  (((vector*)(GC_POINTER(o)))->slot[0])
#define _CDR(o)  (((vector*)(GC_POINTER(o)))->slot[1])
#define _CAAR(o) _CAR(_CAR(o))
#define _CADR(o) _CAR(_CDR(o))
#define _CDDR(o) _CDR(_CDR(o))
#define _CDAR(o) _CDR(_CAR(o))
#define _CADDR(o) _CAR(_CDDR(o))
#define _CDDDR(o) _CDR(_CDDR(o))
#define _CADDDR(o) _CAR(_CDDDR(o))
#define _CDDDDR(o) _CDR(_CDDDR(o))
#define _CADDDDR(o) _CAR(_CDDDDR(o))

DEF_STRUCT(pair,   TAG_PAIR)
DEF_STRUCT(lpair,  TAG_LPAIR)
DEF_STRUCT(lnext,  TAG_LNEXT)
DEF_STRUCT(ldata,  TAG_LDATA)
DEF_STRUCT(aref,   TAG_AREF)
// DEF_STRUCT(box,  TAG_BOX)



#endif
