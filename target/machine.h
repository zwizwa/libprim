#ifndef _MACHINE_H_
#define _MACHINE_H_

/* This header is for machine-specific operations.  Currently it
   supports only Intel x86 architecture (i386 / amd64), and mostly
   serves to identify points in the source code that need special
   attention concerning atomicity. */

typedef void *atomic_ptr;
static inline void atomic_ptr_set(atomic_ptr *p, atomic_ptr v) { *p = v; }
static inline atomic_ptr atomic_ptr_get(atomic_ptr *p) { return *p; }

#endif
