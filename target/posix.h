#ifndef _TARGET_POSIX_H_
#define _TARGET_POSIX_H_

#include <pthread.h>

typedef pthread_mutex_t mutex_t;

static inline void mutex_init(mutex_t *m)   { pthread_mutex_init(m, NULL); }
static inline void mutex_lock(mutex_t *m)   { pthread_mutex_lock(m); }
static inline void mutex_unlock(mutex_t *m) { pthread_mutex_unlock(m); }

static inline int rom_pointer(void *x) { return 0; }

#endif
