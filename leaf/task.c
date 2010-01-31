#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <leaf/task.h>
#include <leaf/port.h>


/* Note that memory allocated by a task cannot be reclaimed.  Either a
   task needs to use only the C stack for storage, or it needs to
   cleanup after itself _and_ be run until exit by the host. */
static void ck_free(ck *x) {
    // printf("task_free(%p)\n", (void*)x);
    free(x->segment);
    free(x);
}
static int ck_write(ck *x, port *p) {
    return port_printf(p, "#<ck:%p:%d>", x, x->size);
}
static void default_jump(ck_class *m) {
    longjmp(m->prompt, 1);
}
static void *default_dont_convert(ck_class *m, void *x) { 
    return x; 
}
static ck_class *ck_class_new(void) {
    ck_class *x  = malloc(sizeof(*x));
    leaf_class_init((leaf_class*)x,
                    (leaf_free_m)ck_free,
                    (leaf_write_m)ck_write);
    x->jump      = default_jump;
    x->to_task   = default_dont_convert;
    x->from_task = default_dont_convert;
    x->base      = NULL; /* filled in on first invoke */
    return x;
}
/* One "class" per thread.  FIXME: this needs to be rethought. */
__thread ck_class *thread_ck_class;
leaf_class *ck_type(void) {
    if (!thread_ck_class) thread_ck_class = ck_class_new();
    return (leaf_class*)thread_ck_class;
}

ck *ck_new(ck_class *ck_class) {
    ck *ck = malloc(sizeof*ck);
    leaf_init(&ck->base, ck_type());
    return ck;
}



/* HOST SIDE */

static inline ck_class *ck_cls(ck *ck) {
    return (ck_class*)leaf_type(&(ck->base));
}

static int resume(ck *ck, char *base, long *pad) {
    /* Grow the stack by recursing.  This code is inspired by:
       http://homepage.mac.com/sigfpe/Computing/continuations.html */
    int margin = (base - (char*)&base) - ck->size;
    if (margin < 0) {
        long pad[30];
        resume(ck, base, pad);
    }
    /* At this point the current call frame doesn't overlap with the
       segment we're about to overwrite. */
    memcpy(base - ck->size, ck->segment, ck->size); 
    longjmp(ck->resume, 1);
}

void ck_invoke_with_class(ck_class *m, ck_start fn, ck **ck, void **value) {
    void *base;
    if (!setjmp(m->prompt)) {
        base = &base;
        if (!m->base) m->base = base; /* init @ first run */
        if (base != m->base) { /* subsequent need same base */
            fprintf(stderr, "ERROR: resume(): wrong base pointer.");
            exit(1);
        }
        m->ck_new = NULL;
        m->channel = m->to_task(m, *value);
        if (!fn) resume(*ck, base, NULL);
        else m->channel = fn(m, *value);
    }
    *ck = m->ck_new;
    *value = m->from_task(m, m->channel);
}
void ck_invoke(ck_start fn, ck **ck, void **value) {
    ck_invoke_with_class((ck_class*)ck_type(), fn, ck, value);
}


/* C TASK SIDE */
static void suspend(ck_class *m) {
    ck *ck = m->ck_new = ck_new(m);
    if (0 == setjmp(ck->resume)) {

        /* Copy C stack segment */
        void *top = &ck;
        ck->size = ((char*)m->base - (char*)top);  /* grows downward */
        ck->segment = malloc(ck->size);
        memcpy(ck->segment, top, ck->size);

        /* Abort to sequencer. */
        m->jump(m);
    }
}
void* ck_yield(ck_class *m, void *value) {
    m->channel = value;
    suspend(m);
    return m->channel;
}


