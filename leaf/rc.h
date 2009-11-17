#ifndef _RC_H_
#define _RC_H_

/* Ref management wrapper. */
typedef void (*rc_free)(void *ctx);
typedef struct {
} rc_class;
typedef struct {
    rc_class *type;
    void *ctx;
    int rc;
    rc_free free;
    _ wrap;
} rc;

/* FIXME: turn this into a proper class object. */
static inline void* rc_type(void) {
    return (void*)0xF002; // dummy class
}

#endif
