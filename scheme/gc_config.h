/* Primitive structs use the top tag bits in the vector's size
   slot. */
#ifdef _LP64
#define GC_VECTOR_TAG_SHIFT 60
#else
#define GC_VECTOR_TAG_MASK 28
#endif
#define GC_VECTOR_TAG_MASK ((1L<<GC_VECTOR_TAG_SHIFT)-1L)
#include "gc.h"

