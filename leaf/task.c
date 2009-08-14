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

/* Note that memory allocated by a task cannot be reclaimed.  Either a
   task needs to use only the C stack for storage, or it needs to
   cleanup after itself _and_ be run until exit by the host. */
static void default_free(ck *x) {
    printf("task_free(%p)\n", x);
    free(x->segment);
    free(x);
}
static void default_jump(ck_class *m) {
    longjmp(m->prompt, 1);
}
static void *default_dont_convert(ck_class *m, void *x) { 
    return x; 
}
ck_class *ck_class_new(void) {
    ck_class *x  = malloc(sizeof(*x));
    x->free      = default_free;
    x->jump      = default_jump;
    x->to_task   = default_dont_convert;
    x->from_task = default_dont_convert;
    x->base      = NULL; // filled in on first invoke
    return x;
}
ck *ck_new(ck_class *ck_class) {
    ck *ck = malloc(sizeof*ck);
    ck->type = ck_class;
    return ck;
}

/* HOST SIDE */
static void resume(ck *_ck, void *base) {
    thread_static ck *ck; ck = _ck;// variable not on C stack.

    /* Copy stack */
    void *sp = ck->type->base - ck->size;

    /* Reserve stack space so function call/return keeps working after
       a part of the stack is overwritten. */
    void *reserve[ck->size / sizeof(void *)];

    memcpy(sp, ck->segment, ck->size);
    /* Here 'reserve' is used as a dummy value to make sure it's not
       optimized away. */
    longjmp(ck->resume, (int)((long)reserve));
}
void ck_invoke(ck_class *m, ck_start fn, ck **ck, void **value) {
    void *base;
    if (!setjmp(m->prompt)) {
        base = &base;
        if (!m->base) m->base = base; // init @ first run
        if (base != m->base) { // subsequent need same base
            fprintf(stderr, "ERROR: resume(): wrong base pointer.");
            exit(1);
        }
        m->ck_new = NULL;
        m->channel = m->to_task(m, *value);
        if (!fn) resume(*ck, base);
        else m->channel = fn(m, *value);
    }
    *ck = m->ck_new;
    *value = m->from_task(m, m->channel);
    return;
}


/* C TASK SIDE */
static void suspend(ck_class *m) {
    ck *ck = m->ck_new = ck_new(m);
    if (0 == setjmp(ck->resume)) {

        /* Copy C stack segment */
        void *top = &ck;
        ck->size = m->base - top;  // grows downward
        ck->segment = malloc(ck->size);
        memcpy(ck->segment, top, ck->size);

        /* Abort to sequencer. */
        m->jump(m);
        exit(1); // not reached
    }
}
void* ck_yield(ck_class *m, void *value) {
    m->channel = value;
    suspend(m);
    return m->channel;
}


