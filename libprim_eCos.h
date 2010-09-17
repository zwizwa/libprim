// quick fix void stubs to get it to build on eCos

typedef int pthread_t[0];
typedef int pthread_attr_t[0];
typedef int pthread_mutex_t[0];
typedef int pthread_cond_t[0];
static inline void pthread_mutex_lock(void *m) {}
static inline void pthread_mutex_unlock(void *m) {}
static inline void pthread_mutex_init(void *m, void *opts) {}
static inline void pthread_cond_init(void *cond, void *opts) {}
static inline void pthread_cond_broadcast(void *cond) {}
static inline void pthread_cond_wait(void *cond, void *mut) {}
static inline void pthread_attr_init(void *attr) {}
static inline void pthread_create(void *thread, void *attr, void *fn, void *arg) {}

static inline int mkstemp(char *name) { return -1; }
static inline void* fdopen(int fd, const char *cmode) { return 0; }
static inline int fileno(void *file) { return -1; }

typedef void* fd_set;
#define FD_SET(fd, set)
#define FD_ISSET(fd, set) 0
#define FD_ZERO(set)

struct timeval {int sec; int usec;};

static inline int select(int fdmax, fd_set in, fd_set out, fd_set err, struct timeval *tv) { return -1; }
static inline int random(void) { return 123; }

static inline void kill(int pid, int sig) { }

#define RTLD_NOW 0
static inline void *dlopen(const char *name, int flags) { return 0; }
static inline char *dlerror(void) { return "not implemented"; }
static inline void dlclose(void *lib) {}
static inline void *dlsym(void *lib, char *name) {return 0;}
