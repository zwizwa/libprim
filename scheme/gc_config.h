/* Primitive structs use the top tag bits in the vector's size
   slot. */

#ifndef _GC_CONFIG_
#define _GC_CONFIG_

/* Scheme uses the top 4 bits to tag records.  
   0000 = vector
   1xxx = continuation frame
   0xxx = other 

   The remaining bits are for vector size.
*/

#ifdef _LP64
#define GC_VECTOR_TAG_SHIFT 60
#define GC_VECTOR_TAG_MASK ((1L<<GC_VECTOR_TAG_SHIFT)-1L)
#else
#define GC_VECTOR_TAG_SHIFT 28
#define GC_VECTOR_TAG_MASK ((1<<GC_VECTOR_TAG_SHIFT)-1)
#endif

#include "gc.h"

#endif

