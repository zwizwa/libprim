#ifndef _TARGET_POSIX_H_
#define _TARGET_POSIX_H_

#include <pthread.h>

typedef pthread_mutex_t mutex_t;

static inline void mutex_init(mutex_t *m)   { pthread_mutex_init(m, NULL); }
static inline void mutex_lock(mutex_t *m)   { pthread_mutex_lock(m); }
static inline void mutex_unlock(mutex_t *m) { pthread_mutex_unlock(m); }

typedef pthread_t thread_t;
typedef void *(*thread_main_t)(void *);

static inline void thread_create_detached(thread_t *t, thread_main_t m, void *x) {
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, 0);
    pthread_create(t, &attr, m, x);
    pthread_attr_destroy(&attr);
}


static inline int rom_pointer(void *x) { return 0; }

#endif
