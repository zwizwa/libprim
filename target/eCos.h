#ifndef _LIBPRIM_ECOS_H_
#define _LIBPRIM_ECOS_H_

/* Stubs to make libprim run on eCos. */

#include <stdlib.h>
#include <cyg/kernel/kapi.h>

#if 0

#include <pthread.h>

#else

/* Mutex maps to cyg_mutex, ignoring attributes. */
typedef cyg_mutex_t pthread_mutex_t;
typedef void pthread_t;
typedef void pthread_attr_t;
static inline void pthread_attr_init(void *attr) {}

static inline void pthread_mutex_lock(pthread_mutex_t *m) {cyg_mutex_lock(m);}
static inline void pthread_mutex_unlock(pthread_mutex_t *m) {cyg_mutex_unlock(m);}
static inline void pthread_mutex_init(pthread_mutex_t *m, void *opts) {cyg_mutex_init(m);}

/* FIXME: condition variables are not implemented. */
typedef int pthread_cond_t[0];
static inline void pthread_cond_init(void *cond, void *opts) {}
static inline void pthread_cond_broadcast(void *cond) {}
static inline void pthread_cond_wait(void *cond, void *mut) {}
static inline void pthread_create(void *thread, void *attr, void *fn, void *arg) {}

#endif


/* Are there any pitfalls here? */
static inline int random(void) { return rand(); }




/* The rest is stubbed out posix stuff that's currently not included
   in the eCos build. */
#if 0

static inline int fileno(void *file) { return -1; }

static inline int mkstemp(char *name) { return -1; }
static inline void* fdopen(int fd, const char *cmode) { return 0; }

typedef void* fd_set;
#define FD_SET(fd, set)
#define FD_ISSET(fd, set) 0
#define FD_ZERO(set)

struct timeval {int sec; int usec;};

static inline int select(int fdmax, fd_set in, fd_set out, fd_set err, struct timeval *tv) { return -1; }
static inline void kill(int pid, int sig) { }

#define RTLD_NOW 0
static inline void *dlopen(const char *name, int flags) { return 0; }
static inline char *dlerror(void) { return "not implemented"; }
static inline void dlclose(void *lib) {}
static inline void *dlsym(void *lib, char *name) {return 0;}

#endif
#endif




