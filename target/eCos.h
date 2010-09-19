#ifndef _LIBPRIM_ECOS_H_
#define _LIBPRIM_ECOS_H_

#include <stdlib.h>
#include <cyg/kernel/kapi.h>

/* Mutex maps to cyg_mutex, ignoring attributes. */
typedef cyg_mutex_t mutex_t;

static inline void mutex_lock(mutex_t *m)   { cyg_mutex_lock(m); }
static inline void mutex_unlock(mutex_t *m) { cyg_mutex_unlock(m); }
static inline void mutex_init(mutex_t *m)   { cyg_mutex_init(m); }


/* Are there any pitfalls here? */
static inline int random(void) { return rand(); }

/* Flash memory? */
// #include <cyg/infra/diag.h>
static inline int rom_pointer(void *x) { 
    // diag_printf("%p\n", x);
    return 0; 
}

#endif


