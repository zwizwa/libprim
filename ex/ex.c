#include "object.h"
#include "port.h"
#include "pair.h"
#include "ex.h"
#include "ex_prims.h_ex_prims"

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
                ex->write(ex, _CAR(o));
                o = _CDR(o);
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


/* Pairs and lambdas are tagged vectors. */
_ _is_vector_type(_ o, long flags) {
    vector *v;
    if ((v = object_to_vector(o)) &&
        (flags == vector_to_flags(v))) { return TRUE; }
    return FALSE;
}

_ _ex_make_symbol(ex *ex, const char *str) {
    return const_to_object((void*)(symbol_from_string(ex->p->symbol_type, str)));
}

void* _ex_unwrap_pointer(ex *ex, void *unwrap, object o){
    void *x = ((object_to_pointer)unwrap)(o, ex);
    if (unlikely(!x)) ex_raise_type_error(ex, o);
    return x;
}
long _ex_unwrap_integer(ex *ex, object o) {
    if ((FALSE == ex_is_integer(ex, o))) 
        return ex_raise_type_error(ex, o);
    return object_to_integer(o);
}
_ _ex_restart(ex *ex) {
    if (ex->top_entries) {
        longjmp(ex->top, 1);
    }
    _ex_printf(ex, "ERROR: attempt restart outside of the main loop.\n");
    ex_trap(ex);
    exit(1);
}

void _ex_overflow(ex *ex, long extra) {
    /* At this point, the heap is compacted, but the requested
       allocation doesn't fit.  We need to grow.  Take at least the
       requested size + grow by a fraction of the total heap. */
    long request = extra + (ex->gc->slot_total/4);
    _ex_printf(ex, ";; gc-overflow %ld:%ld\n", extra, request);
    gc_grow(ex->gc, request);
    _ex_restart(ex);
}   

/* Primitive map to make some primitives easier. */
_ _ex_map1_prim(ex *ex, ex_1 fn, _ l_in) {
    _ res = ex_list_clone(ex, l_in);
    _ l_out = res;
    pair *in, *out;
    for(;;) {
        in  = object_to_pair(l_in);
        out = object_to_pair(l_out);
        if (!in) return res;
        out->car = fn(ex, in->car);
        l_in  = in->cdr;
        l_out = out->cdr;
    }
}
_ _ex_map2_prim(ex *ex, ex_2 fn, _ l_in1, _ l_in2) {
    _ res = ex_list_clone(ex, l_in1);
    _ l_out = res;
    pair *in1, *in2, *out;
    for(;;) {
        in1 = object_to_pair(l_in1);
        in2 = object_to_pair(l_in2);
        out = object_to_pair(l_out);
        if ((!in1) || (!in2)) return res;
        out->car = fn(ex, in1->car, in2->car);
        l_in1 = in1->cdr;
        l_in2 = in2->cdr;
        l_out = out->cdr;
    }
}
