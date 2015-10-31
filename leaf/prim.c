#include <leaf/prim.h>
#include <stdlib.h>

#define unlikely(x) __builtin_expect((long)(x),0)
#define likely(x)   __builtin_expect((long)(x),1)

static int prim_write(prim *x, port *p) {
    return port_printf(p, "#<prim:%p>", prim_fn(x));
}
static void prim_free(prim *s) {
    // NOP
}

static prim_class *type = NULL;
static prim_class *prim_class_new(void) {
    prim_class *p = calloc(1, sizeof(*p));
    leaf_class_init((leaf_class*)p, (leaf_free_m)prim_free, (leaf_write_m)prim_write);
    return p;
}
leaf_class *prim_type(void) {
    if (unlikely(!type)) type = prim_class_new();
    return (void*)type;
}

