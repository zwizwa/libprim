#ifndef _PRIM_TASK_H_
#define _PRIM_TASK_H_

#include <setjmp.h>

/* One-shot and resumable tasks. */

typedef struct _ck ck;
typedef struct _ck_manager ck_manager;
typedef void (*ck_free)(ck *);
typedef void (*ck_jump)(ck *);
typedef void* (*ck_convert)(ck_manager *, void*);

/* It feels wrong to call this `class' because of the data fields. */
struct _ck_manager {
    ck_free free;
    ck_jump jump;
    ck_convert to_task;
    ck_convert from_task;

    /* Temp storage for value transport and control transfer. */
    void *channel;
    ck *ck_new;
    jmp_buf prompt;
};

struct _ck {
    ck_manager *manager;
    jmp_buf resume;
    void *base;
    void *segment;
    int size;
};

ck_manager* ck_manager_new(void);

void ck_invoke(ck **ck, void **value);


#endif
