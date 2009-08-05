/* Primitive structs use the top tag bits in the vector's size
   slot. */
#ifdef _LP64
#define GC_VECTOR_TAG_SHIFT 30
#else
#define GC_VECTOR_TAG_MASK 14
#endif
#define GC_VECTOR_TAG_MASK ((1<<GC_VECTOR_TAG_SHIFT)-1)
#include "gc.h"

