#ifndef _GC_H_
#define _GC_H_

/* Simple stop-and-copy GC for allocating graphs of vectors and atoms.

   The GC uses the following annotations:

   1. VECTOR-SIZE - For a live object, the first element of a struct
      vector_ contains the size.

   2. VECTORS? - Cells are tagged using 2 tag bits.

   3. FINALIZERS - A finalizer on heap location n is a function
      that will be applied to the constant on heap location n+1
      whenever the vector containing the finalizer is no longer
      reachable.

   4. MOVED? - A moved object contains an an object reference in the
      size slot.

   GC is implemented in a header file, since its code depends on
   configuration that changes the vector tags. 

   Note that using this GC in conjuction with C code requires solution
   of 2 problems: pointers will have changed + C stack isn't scanned.
   This keeps the implementation of the GC simple.

   In the Scheme interpreter this is solved by simply restarting each
   primitive after a collection, and making sure that this is possible
   by using purely functional primitives, or always performing side
   effects _after_ allocation.

*/



#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "object.h"

/* Note that GC_INTEGER (which would make integer addition and
   subtraction simpler) can't be 0 when we want NIL == 0 */

typedef struct _gc gc;

typedef void (*gc_finalize)(gc *);
typedef void (*gc_mark_roots)(void *ctx, gc_finalize finalize);
typedef void (*gc_overflow)(void *ctx, long nb_extra);

struct _gc {
    vector  *current;
    long    current_index;
    vector  *old;
    long    old_index;
    long    slot_total;
    long    want;             // last data request
    long    margin;           // grow trigger margin
    gc_mark_roots mark_roots; // (1)
    gc_overflow   overflow;
    void *client_ctx;
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



gc *gc_new(long total, void *ctx, gc_mark_roots mark, 
           gc_overflow  overflow);
void gc_collect(gc *gc);
object gc_mark(gc *gc, object o_old);
int gc_grow(gc *gc, long add_slots);

#define GC_CHENEY 1

void _gc_assert(const char *cond, const char *file, int line);

vector *gc_alloc(gc *gc, long size);
object gc_make_tagged_v(gc *gc, long tag, long slots, va_list ap);
object gc_make_tagged(gc *gc, long tag, long slots, ...);
#endif
