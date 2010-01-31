#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <leaf/task.h>
#include <leaf/port.h>


/* Note that memory allocated by a task cannot be reclaimed.  Either a
   task needs to use only the C stack for storage, or it needs to
   cleanup after itself _and_ be run until exit by the host. */
static void ck_free(ck *x) {
    printf("task_free(%p)\n", (void*)x);
    free(x->segment);
    free(x);
}
static int ck_write(ck *x, port *p) {
    return port_printf(p, "#<ck:%p>", x);
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

/* Context that needs to survive stack switching needs to be stored
   somewhere other than the C stack. */
__thread ck *thread_ck;

static inline ck_class *ck_cls(ck *ck) {
    return (ck_class*)leaf_type(&(ck->base));
}
static void resume(ck *_ck, void *base) {
    void *sp = NULL;
    thread_ck = _ck;  /* variable not on C stack */

    /* Copy stack */
    sp = (void*) ((char*)ck_cls(thread_ck)->base - thread_ck->size);

    /* Reserve stack space so function call/return keeps working after
       a part of the stack is overwritten. */
    void *reserve[thread_ck->size / sizeof(void *)];
    
    fprintf(stderr, "resume: copy %d bytes: %p -> %p\n", thread_ck->size, thread_ck->segment, sp);
    memcpy(sp, thread_ck->segment, thread_ck->size);
    /* Here 'reserve' is used as a dummy value to make sure it's not
       optimized away. */
    fprintf(stderr, "longjmp(resume)\n");
    longjmp(thread_ck->resume, (int)((long)reserve));
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
        if (!fn) resume(*ck, base);
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
        fprintf(stderr, "suspend: copy %d bytes: %p -> %p\n", ck->size, top, ck->segment);
        memcpy(ck->segment, top, ck->size);

        /* Abort to sequencer. */
        m->jump(m);
        exit(1); /* not reached */
    }
}
void* ck_yield(ck_class *m, void *value) {
    m->channel = value;
    suspend(m);
    return m->channel;
}


