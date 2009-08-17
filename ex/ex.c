
/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) use primitives from this expression language EX, which is
   essentially C with dynamic type checking and GC.

   Note that these only work inside 
     - setjump(ex->top) for GC restarts
     - setjump(ex->r.step) for primitive exceptions.
*/

#include "object.h"
#include "port.h"
#include "pair.h"
#include "ex.h"
#include "ex.h_ex_prims"

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


/* PRIMITIVES */
#define EX ex

/* Booleans are GC_CONST */
_ ex_is_bool(ex *ex, _ o) {
    void *x;
    if ((x = object_to_const(o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}
/* The empty list is the NULL pointer */
_ ex_is_null(ex *ex, _ o) {
    if (NIL == o) return TRUE; else return FALSE;
}
_ ex_is_integer(ex *ex, _ o) {
    if (GC_INTEGER == GC_TAG(o)) return TRUE;
    return FALSE;
}
_ ex_is_zero(ex *ex, _ o) {
    long i = CAST_INTEGER(o);
    if (i) return FALSE;
    return TRUE;
}

#define OBJECT_PREDICATE(cast) \
    {if (cast(o, ex)) return TRUE; else return FALSE;}
_ ex_is_symbol(ex *ex, _ o) { OBJECT_PREDICATE(object_to_symbol); }
_ ex_is_prim(ex *ex, _ o)   { OBJECT_PREDICATE(object_to_prim); }


_ ex_is_pair(ex *ex, _ o)    { return _is_vector_type(o, TAG_PAIR); }
_ ex_is_vector(ex *ex, _ o)  { return _is_vector_type(o, TAG_VECTOR); }


/* Arithmetic */
_ ex_add1(ex *ex, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i + 1);
}
_ ex_sub1(ex *ex, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i - 1);
}
_ ex_add(ex *ex, _ a, _ b) {
    long ia = CAST_INTEGER(a);
    long ib = CAST_INTEGER(b);
    return integer_to_object(ia + ib);
}


/* Lists and vectors. */
_ ex_make_vector(ex *ex, _ slots, _ init) {
    long i,n = CAST_INTEGER(slots);
    vector *v = gc_alloc(ex->gc, n);
    for(i=0; i<n; i++) v->slot[i] = init;
    return vector_to_object(v);
}
_ ex_reverse(ex *ex, _ lst) {
    _ rlst = NIL;
    while(FALSE == (IS_NULL(lst))) {
        pair *p = CAST(pair, lst);
        rlst = CONS(p->car, rlst);
        lst  = p->cdr;
    }
    return rlst;
}
// in-place
_ ex_bang_reverse_append(ex *ex, _ lst, _ tail) {
    if (NIL == lst) return tail;
    _ next, last = tail;
    while (NIL != lst) {
        pair *p = CAST(pair, lst);
        next = p->cdr;
        p->cdr = last;
        last = lst;
        lst = next;
    }
    return last;
}
_ ex_bang_reverse(ex *ex, _ lst) {
    return ex_bang_reverse_append(ex, lst, NIL);
}


_ ex_length(ex *ex, _ lst) {
    _ nb;
    _ rest;
    _ex_length_rest(ex, lst, &nb, &rest);
    if (FALSE == IS_NULL(rest)) {
        TYPE_ERROR(lst);
    }
    return nb;
}

// Take n elements from the head of a list and place them in a vector.
_ ex_take_vector(ex *ex, _ n, _ in_lst) {
    _ lst = in_lst;
    long slots = CAST_INTEGER(n);
    vector *v = gc_alloc(ex->gc, slots);
    long i;
    for(i=0; i<slots; i++){
        if (FALSE == IS_PAIR(lst)) return TYPE_ERROR(in_lst);
        pair *p = object_to_pair(lst);
        v->slot[i] = p->car;
        lst = p->cdr;
    }
    return vector_to_object(v);
}
_ ex_list_to_vector(ex *ex, _ lst){
    return ex_take_vector(ex, ex_length(ex, lst), lst);
}


_ ex_cons(ex *ex, _ car, _ cdr) {
    vector *v = gc_alloc(ex->gc, 2);
    vector_set_flags(v, TAG_PAIR);
    v->slot[0] = car;
    v->slot[1] = cdr;
    return vector_to_object(v);
}

_ ex_car(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->car; }
_ ex_cdr(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->cdr; }
_ ex_cadr(ex *ex, _ o) { pair *p = CAST(pair, ex_cdr(ex, o)); return p->car; }
_ ex_cdar(ex *ex, _ o) { pair *p = CAST(pair, ex_car(ex, o)); return p->cdr; }
_ ex_caar(ex *ex, _ o) { pair *p = CAST(pair, ex_car(ex, o)); return p->car; }




_ ex_find_slot(ex *ex, _ E, _ var) {
    if (TRUE == ex_is_null(ex, E)) return FALSE;
    _ slot = CAR(E);
    _ name = CAR(slot);
    if (name == var) return slot;
    else return ex_find_slot(ex, CDR(E), var);
}
_ ex_find(ex *ex, _ E, _ var) {
    _ rv = ex_find_slot(ex, E, var);
    if (FALSE == IS_PAIR(rv)) return FALSE;
    return CDR(rv);
}
_ ex_find2(ex *ex, _ E_local, _ E_toplevel, _ var) {
    _ rv;
    if (FALSE != (rv = ex_find(ex, E_local, var))) return rv;
    return ex_find(ex, E_toplevel, var);
}
_ ex_unfind(ex *ex, _ E, _ val) {
    if (NIL == E) return FALSE;
    if (CDAR(E) == val) return CAAR(E);
    return ex_unfind(ex, CDR(E), val);
}

_ ex_make_true(ex *ex)  { return TRUE; }
_ ex_make_false(ex *ex) { return FALSE; }
_ ex_make_void(ex *ex)  { return VOID; }

_ ex_is_eq(ex *ex, _ a, _ b) {
    if (a == b) return TRUE;
    return FALSE;
}
_ ex_is_list(ex *ex, _ o) {
    if(TRUE==IS_NULL(o)) return TRUE;
    if(FALSE==IS_PAIR(o)) return FALSE;
    return ex_is_list(ex, CDR(o));
}
static _ *vector_index(ex *ex, _ vec, _ n) {
    vector *v = CAST(vector, vec);
    long index = CAST_INTEGER(n);
    if ((index < 0) || (index >= vector_size(v))) ERROR("ref", n);
    return &v->slot[index];
}
_ ex_vector_ref(ex *ex, _ vec, _ n) {
    return *vector_index(ex, vec, n);
}
_ ex_bang_vector_set(ex *ex, _ vec, _ n, _ val) {
    *vector_index(ex, vec, n) = val;
    return VOID;
}
_ ex_env_set(ex *ex, _ E, _ var, _ value) {
    _ rv = ex_find_slot(ex, E, var);
    if (FALSE == IS_PAIR(rv)) return FALSE;
    _CDR(rv)=value;
    return VOID;
}
_ ex_env_def(ex *ex, _ E, _ var, _ value) {
    _ slot = ex_find_slot(ex, E, var);
    if (FALSE == slot) {
        return CONS(CONS(var,value),E);
    }
    else {
        _CDR(slot) = value;
        return E;
    }
}
_ ex_list_clone(ex *ex, _ lst) {
    if (NIL == lst) return lst;
    _ res = CONS(VOID, NIL);
    pair *in,*out;
    out = object_to_pair(res);
    for(;;) {
        in  = CAST(pair, lst);
        if (NIL == in->cdr) return res;
        out->cdr = CONS(VOID,NIL);
        out = object_to_pair(out->cdr);
        lst = in->cdr;
    }
}
_ ex_map1_prim(ex *ex, _ fn, _ l_in) {
    prim *p = CAST(prim, fn);
    if (1 != p->nargs) ex_raise_nargs_error(ex, fn);
    return _ex_map1_prim(ex, (ex_1)(p->fn), l_in);
}
_ ex_map2_prim(ex *ex, _ fn, _ l_in1, _ l_in2) {
    prim *p = CAST(prim, fn);
    if (2 != p->nargs) ex_raise_nargs_error(ex, fn);
    return _ex_map2_prim(ex, (ex_2)(p->fn), l_in1, l_in2);
}



_ ex_post(ex* ex, _ o) {
    if (VOID != o) {
        ex->write(ex, o);
        _ex_printf(EX, "\n");
    }
    return VOID;
}


/* ERRORS */

_ ex_trap(ex *ex) {
    kill(getpid(), SIGTRAP);
    return VOID;
}

_ ex_raise_error(ex *ex, _ tag_o, _ arg_o) {
    ex->error_tag = tag_o;
    ex->error_arg = arg_o;
    // if (sym_o != SYMBOL("halt")) ex_trap(ex);
    if (ex->entries) longjmp(ex->r.step, EXCEPT_ABORT);
    _ex_printf(ex, "ERROR (outside of VM): ");
    ex->write(ex, tag_o); _ex_printf(ex, ": ");
    ex->write(ex, arg_o); _ex_printf(ex, "\n");
    TRAP();
    exit(1);
}

_ ex_raise_type_error(ex *ex, _ arg_o) {
    return ex_raise_error(ex, SYMBOL("type"), arg_o);
}
_ ex_raise_nargs_error(ex *ex, _ arg_o) {
    return ex_raise_error(ex, SYMBOL("nargs"), arg_o);
}
