#ifndef _TASK_H_
#define _TASK_H_

#include <setjmp.h>

/* One-shot and resumable tasks. */

typedef struct _ck ck;
typedef struct _ck_class ck_class;
typedef void (*ck_msg)(ck *);

struct _ck_class {
    ck_msg free;
    ck_msg jump;  // filled in by client
};


struct _ck {
    ck_class *type;
    void *value;
    jmp_buf resume;
    void *base;
    void *segment;
    int size;
};

#endif
