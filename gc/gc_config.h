

#ifndef GC_VECTOR_TAG_MASK
#ifdef _LP64
#define GC_VECTOR_TAG_MASK 0xFFFFFFFFFFFFFFFFL
#else
#define GC_VECTOR_TAG_MASK 0xFFFFFFFF
#endif
#endif

#warning default gc_config.h
#include "gc.h"

