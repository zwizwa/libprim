/* Single function: can be overridden at link time. */
#include <leaf/leaf.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
void leaf_log(const char *msg, ...) {
    va_list vl;
    va_start(vl,msg);
    vfprintf(stderr, msg, vl);
    fprintf(stderr, "\n");
    va_end(vl);
}

