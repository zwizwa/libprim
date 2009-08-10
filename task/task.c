#include <stdlib.h>
#include <string.h>
#include "task.h"


/* Context that needs to survive stack switching needs to be stored
   somewhere other than the C stack.  To be (preemptibly) thread-safe
   this needs to be thread-local storage.  In an -O build the variable
   will (most likely?) be in a register.  (Note that the `register'
   attribute doesn't guarantee this!).  In debug mode we use a static
   var, which means it is not thread-safe! */
#ifndef TASK_DEBUG
#warning TASK_DEBUG not defined.  Default to non-thread-safe behaviour.
#define TASK_DEBUG 1
#endif

#if TASK_DEBUG
#define thread_static static
#else
#define thread_static register
#endif


/* Resumable continuations only need a base pointer and a setjmp to
   unwind the stack.  */


static void default_free(ck *ck) {
    free(ck->segment);
    free(ck);
}

/* Jump to primitive invocation point. */
static void default_jump(ck *ck, void *value) {
    ck_manager *x = ck->manager;
    x->channel = value;
    longjmp(x->prompt, 1);
}

ck_manager *ck_manager_new(void) {
    ck_manager *x = malloc(sizeof(*x));
    x->free = default_free;
    x->jump = default_jump;
    return x;
}


// invoke continuation

void ck_resume(ck *_ck, void *value) {
    thread_static ck *ck; // variable not on C stack.
    ck = _ck; 
    ck->manager->channel = value;
    void *sp = ck->base - ck->size;

    /* Reserve stack space so function call/return keeps working after
       a part of the stack is overwritten. */
    void *reserve[ck->size / sizeof(void *)];

    memcpy(sp, ck->segment, ck->size);
    /* Here 'reserve' is used as a dummy value to make sure it's not
       optimized away. */
    longjmp(ck->resume, (int)((long)reserve));
}

ck *ck_new(ck_manager *ck_manager) {
    ck *ck = malloc(sizeof*ck);
    ck->manager = ck_manager;
    return ck;
}

// create continuation
void* ck_suspend(ck_manager *ck_manager, void *base, void *value) {
    ck *ck = ck_new(ck_manager);
    if (0 == setjmp(ck->resume)) {

        /* copy C stack segment */
        void *top = &ck;
        ck->base = base;
        ck->size = base - top;  // grows downward
        ck->segment = malloc(ck->size);
        memcpy(ck->segment, top, ck->size);

        /* abort to sequencing context */
        ck_manager->jump(ck,value);
        exit(1); // not reached
    }
    else {
        /* Resuming.  We don't need to do anything to the ck struct
           since it is managed by the GC. (It can be used multiple
           times.)  */
        /* The `sc' arg gets borked so we take the one from `ck'. */
        return ck_manager->channel;
    }
}
