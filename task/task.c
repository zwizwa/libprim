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


void ck_free(ck *ck) {
    free(ck->segment);
    free(ck);
}

// invoke continuation

void ck_resume(ck *_ck, void *value) {
    thread_static ck *ck; // variable not on C stack.
    ck = _ck; 
    ck->value = value;
    void *sp = ck->base - ck->size;

    /* Reserve stack space so function call/return keeps working after
       a part of the stack is overwritten. */
    void *reserve[ck->size / sizeof(void *)];

    memcpy(sp, ck->segment, ck->size);
    longjmp(ck->resume, 1);
}

ck *ck_new(ck_class *ck_class) {
    ck *ck = malloc(sizeof*ck);
    ck->type = ck_class;
}

// create continuation
void* ck_suspend(ck_class *ck_class, void *base, void *value) {
    ck *ck = ck_new(ck_class);
    if (0 == setjmp(ck->resume)) {
        /* copy C stack segment */
        void *top = &ck;
        ck->base = base;
        ck->size = base - top;  // grows downward
        ck->segment = malloc(ck->size);
        memcpy(ck->segment, top, ck->size);

        /* abort to interpreter */
        ck_class->jump(ck);
    }
    else {
        /* Resuming.  We don't need to do anything to the ck struct
           since it is managed by the GC. (It can be used multiple
           times.)  */
        /* The `sc' arg gets borked so we take the one from `ck'. */
        return ck->value;
    }
}
