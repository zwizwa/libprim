/* Single function: can be overridden at link time. */
#include <leaf/leaf.h>
#include <stdlib.h>

void leaf_assert(bool b, const char *file, int line, const char *msg) {
    if (!b) {
        LEAF_LOG("%s:%d: LEAF_ASSERT(%s) failed", file, line, msg);
        leaf_trap(); // For breakpoint
        exit(1);
    }
}

