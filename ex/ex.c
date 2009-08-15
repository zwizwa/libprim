#include "object.h"
#include "port.h"
#include "pair.h"
#include "ex.h"

object _ex_write_vector(ex *ex, const char *type, vector *v) {
    port *p = ex->port(ex);
    long i,n = vector_size(v);
    port_printf(p, "#%s(", type);
    for(i=0;i<n;i++){
        ex->write(ex, v->slot[i]);
        if (i != n-1) port_printf(p, " ");
    }
    port_printf(p, ")");
    return VOID;
}
object ex_write(ex *ex, object o) {
    port *p = ex->port(ex);
    vector *v;
    void *x;
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
            return _ex_write_vector(ex, "", v);
        }
        if (TAG_PAIR == flags) {
            port_printf(p, "(");
            for(;;) {
                ex->write(ex, CAR(o));
                o = CDR(o);
                if (NIL == o) {
                    port_printf(p, ")");
                    return VOID;
                }
                if (!object_to_pair(o)) {
                    port_printf(p, " . ");
                    ex->write(ex, o);
                    port_printf(p, ")");
                    return VOID;
                }
                port_printf(p, " ");
            }
        }
        //if (TAG_BOX == flags) {
        //    return object_write_vector(ex, "box", v);
        // }
        if (TAG_AREF == flags) {
            return _ex_write_vector(ex, "aref", v);
        }
    }
    /* Opaque leaf types */
    if ((x = object_struct(o, ex->p->symbol_type))) {
        symbol *s = (symbol*)x;
        port_printf(p, "%s", s->name);
        return VOID;
    }
    if ((x = object_struct(o, ex->p->prim_type))) {
        prim *pr = (prim*)x;
        port_printf(p, "#prim<%p:%ld>", (void*)(pr->fn),pr->nargs);
        return VOID;
    }
    if ((x = object_struct(o, ex->p->port_type))) {
        port *prt = (port*)x;
        if (prt->name) {
            port_printf(p, "#port<%s>", prt->name);
        }
        else {
            port_printf(p, "#port<%p>", prt->stream);
        }
        return VOID;
    }
    if ((x = object_struct(o, ex->p->rc_type))) {
        rc *r = (rc*)x;
        port_printf(p, "#rc:");
        ex->write(ex, const_to_object(r->ctx));  // foefelare
        port_printf(p, ":%d", (int)(r->rc));
        return VOID;
    }
    if ((x = object_to_fin(o))) {
        port_printf(p, "#fin");
        // port_printf(p, "#fin<%p:%p>", x, *((void**)x)); // do we care?
        return VOID; 
    }
    return FALSE;
}

// types_add(types *m, void *type) {}
_ _ex_printf(ex *ex, const char *fmt, ...) {
    int rv;
    port *p = ex->port(ex);
    va_list ap; va_start(ap, fmt);
    rv = port_vprintf(p, fmt, ap);
    va_end(ap);
    return VOID;
}
