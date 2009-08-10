#ifndef _PRIM_TASK_H_
#define _PRIM_TASK_H_

#include <setjmp.h>

/* One-shot and resumable tasks. */

typedef struct _ck ck;
typedef struct _ck_manager ck_manager;
typedef void* (*ck_start)(ck_manager *, void *);
typedef void (*ck_free)(ck *);
typedef void (*ck_jump)(ck_manager *);
typedef void* (*ck_convert)(ck_manager *, void*);

/* It feels wrong to call this `class' because of the data fields. */
struct _ck_manager {
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
    ck_manager *manager;
    jmp_buf resume;
    void *segment;
    int size;
};

ck_manager* ck_manager_new(void);

// C side
void *ck_yield(ck_manager *m, void *value);

// Host side
void ck_invoke(ck_manager *m, ck_start fn, ck **ck, void **value);



#endif
