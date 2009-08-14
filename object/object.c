#include "object.h"
#include "port.h"
#include "pair.h"
#include "mem.h"


object object_write_vector(const char *type, vector *v, port *p, mem *m,
                           object_write_delegate fn, void *ctx) {
    long i,n = vector_size(v);
    port_printf(p, "#%s(", type);
    for(i=0;i<n;i++){
        fn(ctx, v->slot[i]);
        if (i != n-1) port_printf(p, " ");
    }
    port_printf(p, ")");
    return VOID;
}

struct _write_delegate_ {
    port *p;
    mem *m;
};
static object _write_delegate(struct _write_delegate_ *ctx, object ob) {
    return object_write(ob, ctx->p, ctx->m, NULL, NULL);
}
object object_write(object o, port *p, mem *m,
                    object_write_delegate fn, void *ctx) {
    struct _write_delegate_ _ctx = {p,m};
    vector *v;
    void *x;
    if (!fn) { 
        fn = (object_write_delegate)_write_delegate;
        ctx = &_ctx;
    }
    
    if (TRUE  == o) { port_printf(p, "#t"); return VOID; }
    if (FALSE == o) { port_printf(p, "#f"); return VOID; }
    if (GC_INTEGER == GC_TAG(o)) {
        port_printf(p, "%ld", object_to_integer(o));
        return VOID;
    }
    if (VOID == o) { port_printf(p, "#<void>"); return VOID; }
    if (NIL == o) {
        port_printf(p, "()");
        return VOID;
    }
    if ((v = object_to_vector(o))) {
        long flags = object_get_vector_flags(o);
        if (TAG_VECTOR == flags) { 
            return object_write_vector("", v, p, m, fn, ctx);
        }
        if (TAG_PAIR == flags) {
            port_printf(p, "(");
            for(;;) {
                fn(ctx, CAR(o));
                o = CDR(o);
                if (NIL == o) {
                    port_printf(p, ")");
                    return VOID;
                }
                if (!object_to_pair(o)) {
                    port_printf(p, " . ");
                    fn(ctx, o);
                    port_printf(p, ")");
                    return VOID;
                }
                port_printf(p, " ");
            }
        }
    }
    /* Opaque leaf types */
    if ((x = object_struct(o, m->symbol_type))) {
        symbol *s = (symbol*)x;
        port_printf(p, "%s", s->name);
        return VOID;
    }
    if ((x = object_struct(o, m->prim_type))) {
        prim *pr = (prim*)x;
        port_printf(p, "#prim<%p:%ld>", (void*)(pr->fn),pr->nargs);
        return VOID;
    }
    if ((x = object_struct(o, m->port_type))) {
        port *prt = (port*)x;
        if (prt->name) {
            port_printf(p, "#port<%s>", prt->name);
        }
        else {
            port_printf(p, "#port<%p>", prt->stream);
        }
        return VOID;
    }
    return FALSE;
}


