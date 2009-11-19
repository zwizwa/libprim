#include <stdlib.h>
#include <leaf/channel.h>

static int channel_write(channel *x, port *p) {
    return port_printf(p, "#<channel>");
}
/* Note that a channel always has exactly two references.  Teardown
   follows the following protocol: The writing side will signal EOF by
   writing a NULL object, upon which the reading end frees the
   structure. */
static void channel_free(channel *x, port *p) {
    if (x->object) leaf_free(x->object);
    free(x);
}

LEAF_SIMPLE_TYPE(channel)

leaf_object *channel_get(channel *x) {
    return NULL;
}
int channel_put(channel *x, leaf_object *object) {
    return -1;
}
int channel_get_would_block(channel *x) {
    return -1;
}
int channel_put_would_block(channel *x) {
    return -1;
}
