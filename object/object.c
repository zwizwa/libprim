#include "object.h"
#include "port.h"
#include "pair.h"


object object_write_vector(const char *type, vector *v, port *p,
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



static object _write_delegate(port *p, object ob) {
    return object_write(ob, p, NULL, NULL);
}
object object_write(object o, port *p,
                    object_write_delegate fn, void *ctx) {
    vector *v;
    if (!fn) {
        fn = (object_write_delegate)_write_delegate;
        ctx = p;
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
            return object_write_vector("", v, p, fn, ctx);
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
    return FALSE;
}
