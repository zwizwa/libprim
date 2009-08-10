#include <stdlib.h>
#include <stdio.h>
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
static void default_jump(ck *ck) {
    longjmp(ck->manager->prompt, 1);
}

/* Data conversion. */
static void *default_dont_convert(ck_manager *m, void *x) { return x; }

ck_manager *ck_manager_new(void) {
    ck_manager *x = malloc(sizeof(*x));
    x->free = default_free;
    x->jump = default_jump;
    x->to_task   = default_dont_convert;
    x->from_task = default_dont_convert;
    return x;
}


// invoke continuation
static void resume(ck *_ck, void *base, void *value) {
    thread_static ck *ck; ck = _ck;// variable not on C stack.
    ck_manager *m = ck->manager;

    if (base != ck->base) {
        fprintf(stderr, "ERROR: resume(): wrong base pointer.");
        exit(1);
    }

    /* Transport the value after conversion. */
    m->channel = m->to_task(m, value);

    /* Copy stack */
    void *sp = ck->base - ck->size;

    /* Reserve stack space so function call/return keeps working after
       a part of the stack is overwritten. */
    void *reserve[ck->size / sizeof(void *)];

    memcpy(sp, ck->segment, ck->size);
    /* Here 'reserve' is used as a dummy value to make sure it's not
       optimized away. */
    longjmp(ck->resume, (int)((long)reserve));
}

void ck_invoke(ck **ck, void **value) {
    ck_manager *m = (*ck)->manager;
    if (setjmp(m->prompt)) {
        resume(*ck, &m, *value);
        // Normal return;
        *ck = NULL;
    }
    else {
        // Suspended return;
        *ck = m->ck_new;
    }
    *value = m->from_task(m, m->channel);
    return;
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
        ck_manager->channel = value;
        ck_manager->jump(ck);
        exit(1); // not reached
    }
    else {
        /* Resuming... */
        return ck_manager->channel;
    }
}
