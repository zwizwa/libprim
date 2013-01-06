#ifndef _ZL_CONFIG_H_
#define _ZL_CONFIG_H_

#if 0

/* Tie ZL into mothership. */
void post(const char *msg, ...);
#define ZL_LOG(format, ...)    post("pdp_" format, __VA_ARGS__)
#define ZL_PERROR(format, ...) perror("pdp_" format, __VA_ARGS__)

#else

// #warning ZL running on plain libc

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
static inline void ZL_LOG(const char *msg, ...) {
    va_list vl;
    va_start(vl,msg);
    // fprintf(stderr, "zl_");
    vfprintf(stderr, msg, vl);
    fprintf(stderr, "\n");
    va_end(vl);
}
#define ZL_PERROR perror

#endif


#endif
