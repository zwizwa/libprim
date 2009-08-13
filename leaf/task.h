#ifndef _PRIM_TASK_H_
#define _PRIM_TASK_H_

#include <setjmp.h>

/* One-shot and resumable tasks. */

typedef struct _ck ck;
typedef struct _ck_class ck_class;
typedef void* (*ck_start)(ck_class *, void *);
typedef void (*ck_free)(ck *);
typedef void (*ck_jump)(ck_class *);
typedef void* (*ck_convert)(ck_class *, void*);

/* It feels wrong to call this `class' because of the data fields. */
struct _ck_class {
    ck_free free;
    ck_jump jump;
    ck_convert to_task;
    ck_convert from_task;
    void *base;  // this needs to be the same for all tasks!

    /* Temp storage for value transport and control transfer. */
    void *channel;
    ck *ck_new;
    jmp_buf prompt;
};

struct _ck {
    ck_class *type;
    jmp_buf resume;
    void *segment;
    int size;
};

ck_class* ck_class_new(void);

// C side
void *ck_yield(ck_class *m, void *value);

// Host side
void ck_invoke(ck_class *m, ck_start fn, ck **ck, void **value);



#endif
