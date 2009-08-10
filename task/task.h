#ifndef _TASK_H_
#define _TASK_H_

#include <setjmp.h>

/* One-shot and resumable tasks. */

typedef struct _ck ck;
typedef struct _ck_manager ck_manager;
typedef void (*ck_free)(ck *);
typedef void (*ck_jump)(ck *, void *);

/* It feels wrong to call this `class' because of the data fields. */
struct _ck_manager {
    ck_free free;
    ck_jump jump;

    /* Temp storage for value transport and control transfer. */
    void *channel;
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


#endif
