
/* Both SC (Scheme) and PF (Concatenative language with linear core
   memory) use primitives from this expression language EX, which is
   essentially C with dynamic type checking and GC.

   Note that these only work inside
     - setjump(ex->top) for GC restarts
     - setjump(ex->r.step) for primitive exceptions.
*/

/*
  FIXME: CONS in this file should sometimes be nonlinear cons (both
  for SC and PF) and somtimes linear cons (for PF).

 */

#include "config.h"
#include <ex/ex.h>


object _ex_write_vector(ex *ex, const char *type, vector *v) {
    port *p = ex->port(ex);
    long i,n = vector_size(v);
    const char *close = NULL;
    if (type) {
        port_printf(p, "#<%s:", type);
        close = ">";
    }
    else {
        port_printf(p, "#(", type);
        close = ")";
    }
    for(i=0;i<n;i++){
        ex->write(ex, v->slot[i]);
        if (i != n-1) port_printf(p, " ");
    }
    port_printf(p, "%s", close);
    return VOID;
}

char *object_to_cstring(ex *ex, _ ob) {
    bytes *b = object_to_bytes(ex, ob);
    if (!b) return NULL;
    return cstring_from_bytes(b);
}


/* FIXME: It's probably best to unify this with sequence printing,
   where all the elements in the list are printed by name, and then
   modify sequence printing to use the unquoting syntax.*/
void _ex_prefix_k(ex *ex, _ ob) {
    void *x = NULL;
    long flags = object_get_vector_flags(ob);
    if (TAG_LDATA == flags) x = "'";
    else if (TAG_LNEXT == flags) x = ",";
    if (x) _ex_printf(ex, x);
}

void *object_to_lk(ex *ex, _ ob) {
    void *x = object_to_ldata(ex, ob);
    if (!x) x = object_to_lnext(ex, ob);
    return x;
}
typedef void* (*object_to)(_);

_ _ex_make_file_port(ex *ex, FILE *f, const char *name) {
    return _ex_leaf_to_object(ex, (leaf_object *)port_file_new(f, name));
}

// This has proper EX semantics, but you most probably want to override it.
object _ex_write(ex *ex, object o) {
    port *p = ex->port(ex);
    vector *v;
    void *x;
    leaf_object *l;

    /* If an aref object's class has a lowlevel write method defined, call it. */
    if ((l = ex->object_to_leaf(ex, o))) {
        leaf_write(l, p);
        return VOID;
    }

    if (TRUE  == o) { port_printf(p, "#t"); return VOID; }
    if (FALSE == o) { port_printf(p, "#f"); return VOID; }
    if (EOF_OBJECT  == o) { port_printf(p, "#eof"); return VOID; }
    if (VOID == o) { port_printf(p, "#<void>"); return VOID; }
    if (NIL == o) {
        port_printf(p, "()");
        return VOID;
    }
    if (GC_INTEGER == GC_TAG(o)) {
        port_printf(p, "%ld", object_to_integer(ex, o));
        return VOID;
    }
    if ((v = object_to_vector(ex, o))) {
        object_to is_obj;
        long flags = object_get_vector_flags(o);
        if (TAG_VECTOR == flags) {
            return _ex_write_vector(ex, NULL, v);
        }

        // FIXME: handle one type consed to another!
        if ((TAG_PAIR  == flags) ||
            (TAG_LPAIR == flags) ||
            (TAG_LDATA == flags) ||
            (TAG_LNEXT == flags)) {
            char *LP,*RP;
            if ((TAG_PAIR == flags)) {
                LP="("; RP=")";
                is_obj = (object_to)object_to_pair;
            }
            else if ((TAG_LPAIR == flags)) {
                LP="{"; RP="}";
                is_obj = (object_to)object_to_lpair;
            }
            else {
                LP="<"; RP=">";
                is_obj = (object_to)object_to_lk;
            }
            port_printf(p, LP);
            for(;;) {

                _ex_prefix_k(ex, o);
                ex->write(ex, _CAR(o));
                o = _CDR(o);
                if (NIL == o) {
                    port_printf(p, RP);
                    return VOID;
                }
                if (!(is_obj(o))) {
                    port_printf(p, " . ");
                    ex->write(ex, o);
                    port_printf(p, RP);
                    return VOID;
                }
                port_printf(p, " ");
            }
        }
        //if (TAG_BOX == flags) {
        //    return object_write_vector(ex, "box", v);
        // }
        if (TAG_GENSYM == flags) {
            port_printf(p, "#<gensym:%p>", (void*)o);
            return VOID;
        }
        if (TAG_AREF == flags) {
            aref *a = (aref *)v;
            if (a->object == NIL) {
                /* Wrapped native objects supporting explicit resource
                   management (i.e. channels) can produce empty
                   references. */
                port_printf(p, "#<defunct>");
                return VOID;
            }
            else {
                return _ex_write_vector(ex, "aref", v);
            }
        }
    }
    /* Opaque leaf types */
    if ((x = object_struct(ex, o, symbol_type()))) {
        symbol *s = (symbol*)x;
        port_printf(p, "%s", s->name);
        return VOID;
    }
    if ((x = object_struct(ex, o, prim_type()))) {
        prim *pr = (prim*)x;
        port_printf(p, "#<prim:%p:%ld>", (void*)(pr->fn),pr->nargs);
        return VOID;
    }
    if ((x = object_to_fin(ex, o))) {
        port_printf(p, "#fin");
        // port_printf(p, "#<fin:%p:%p>", x, *((void**)x)); // do we care?
        return VOID;
    }
    if ((x = object_to_const(ex, o))) {
        return _ex_printf(ex, "#<data:%p>", x);
    }
    return _ex_printf(ex, "#<object:%p>",(void*)o);
}

// types_add(types *m, void *type) {}
_ _ex_printf(ex *ex, const char *fmt, ...) {
    // int rv;
    port *p = ex->port(ex);
    if (p) {
        va_list ap; va_start(ap, fmt);
        // rv =
        port_vprintf(p, fmt, ap);
        va_end(ap);
    }
    return VOID;
}


/* Pairs and lambdas are tagged vectors. */
_ _is_vector_type(ex *ex, _ o, long flags) {
    vector *v;
    if ((v = object_to_vector(ex, o)) &&
        (flags == vector_to_flags(v))) { return TRUE; }
    return FALSE;
}

_ ex_struct_to_vector(ex *ex, _ strct) {
    vector *s = object_to_vector(ex, strct);
    if (!s) return ex_raise_type_error(ex, strct);
    int i,n = vector_size(s);
    vector *v = gc_alloc(ex->gc, n);
    for(i=0; i<n; i++) v->slot[i] = s->slot[i];
    return vector_to_object(v);
}

_ _ex_make_symbol(ex *ex, const char *str) {
    return const_to_object((void*)(symbol_from_cstring(str)));
}
_ ex_gensym(ex *ex) {
    vector *v = gc_alloc(ex->gc, 0);
    vector_set_flags(v, TAG_GENSYM);
    return vector_to_object(v);
}

long _ex_unwrap_integer(ex *ex, object o) {
    if ((FALSE == ex_is_integer(ex, o)))
        return ex_raise_type_error(ex, o);
    return object_to_integer(ex, o);
}

void _ex_overflow(ex *ex, long extra) {
    /* At this point, the heap is compacted, but the requested
       allocation doesn't fit.  We need to grow.  Take at least the
       requested size + grow by a fraction of the total heap. */
    long request = extra + (ex->gc->slot_total/4);
    GC_DEBUG { _ex_printf(ex, ";; gc-overflow %ld:%ld\n", extra, request); }
    gc_grow(ex->gc, request);
    _ex_restart(ex);
}

/* Primitive map to make some primitives easier.  In these functions
   ex_list_clone() will enable restarts before any primitive code is
   executed, making it safe for it to have side effect if allocation
   is bounded. */
_ _ex_map1_prim(ex *ex, ex_1 fn, _ l_in) {
    _ res = ex_list_clone(ex, l_in);
    _ l_out = res;
    pair *in, *out;
    for(;;) {
        in  = object_to_pair(ex, l_in);
        out = object_to_pair(ex, l_out);
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
        in1 = object_to_pair(ex, l_in1);
        in2 = object_to_pair(ex, l_in2);
        out = object_to_pair(ex, l_out);
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
_ ex_is_boolean(ex *ex, _ o) {
    void *x;
    if ((x = object_to_const(ex, o)) &&
        (0 == (((long)x) & (~TRUE)))) { return TRUE; }
    return FALSE;
}
_ ex_not(ex *ex, _ o) {
    if (FALSE == o) return TRUE;
    else return FALSE;
}

/* The empty list is the NULL pointer */
_ ex_is_null(ex *ex, _ o) {
    if (NIL == o) return TRUE; else return FALSE;
}
_ ex_is_void(ex *ex, _ o) {
    if (VOID == o) return TRUE; else return FALSE;
}_ ex_is_eof_object(ex *ex, _ o) {
    if (EOF_OBJECT == o) return TRUE; else return FALSE;
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
_ _ex_make_inexact(ex *ex, double d) {
    return _ex_leaf_to_object(ex, inexact_new(d));
}
_ ex_is_inexact(ex *ex, _ o) {
    return (object_to_inexact(ex, o)) ? TRUE : FALSE;
}


#define OBJECT_PREDICATE(cast) \
    {if (cast(ex, o)) return TRUE; else return FALSE;}

_ ex_is_symbol(ex *ex, _ o) {
    if ((object_to_symbol(ex, o)) ||
        (object_to_gensym(ex, o))) return TRUE; else return FALSE;
}
_ ex_is_prim(ex *ex, _ o)   { OBJECT_PREDICATE(object_to_prim); }


_ ex_is_pair(ex *ex, _ o)    { return _is_vector_type(ex, o, TAG_PAIR); }
_ ex_is_vector(ex *ex, _ o)  { return _is_vector_type(ex, o, TAG_VECTOR); }

/* Strings. */
_ _ex_make_string(ex *ex, const char *str) {
    return _ex_leaf_to_object(ex, bytes_from_cstring(str));
}
_ _ex_make_qstring(ex *ex, const char *str) {
    return _ex_leaf_to_object(ex, bytes_from_qcstring(str));
}
_ _ex_make_bytes(ex *ex, int size) {
    return _ex_leaf_to_object(ex, bytes_new(size, size));
}
_ ex_number_to_string(ex *ex, _ num) {
    char str[20];
    sprintf(str, "%ld", (long int)CAST_INTEGER(num));
    return _ex_make_string(ex, str);
}
static char *bytes_ref(ex *ex, _ ob_bytes, _ ob_index) {
    bytes *b = CAST(bytes, ob_bytes);
    int i = CAST_INTEGER(ob_index);
    if ((i < 0) || (i >= b->size)) return (char*)ERROR("index", ob_index);
    return &b->bytes[i];

}
_ ex_bytes_ref(ex *ex, _ ob_bytes, _ ob_index) {
    return integer_to_object(*bytes_ref(ex, ob_bytes, ob_index));
}
_ ex_bang_bytes_set(ex *ex, _ ob_bytes, _ ob_index, _ ob_value) {
    *bytes_ref(ex, ob_bytes, ob_index) = CAST_INTEGER(ob_value);
    return VOID;
}
_ ex_make_bytes(ex *ex, _ ob) {
    int size = CAST_INTEGER(ob);
    if (size <= 0) return INVALID(ob);
    return _ex_make_bytes(ex, size);
}
_ ex_vector_to_bytes(ex *ex, _ ob) {
    vector *v = CAST(vector, ob);
    int i, N = vector_size(v);
    bytes *b = bytes_buffer_new(N+1);
    char *c = bytes_allot(b, N);
    for (i=0; i<N; i++) {
        c[i] = object_to_integer(ex, v->slot[i]);  // don't leak
    }
    return _ex_leaf_to_object(ex, b);
}
_ ex_bytes_init(ex *ex, _ ob_bytes, _ ob_int) {
    bytes *b = CAST(bytes, ob_bytes);
    int fill = CAST_INTEGER(ob_int);
    memset(b->bytes, fill, b->size);
    return VOID;
}
_ ex_symbol_to_string(ex *ex, _ sym) {
    symbol *s = CAST(symbol, sym);
    return _ex_make_string(ex, s->name);
}
_ ex_string_to_symbol(ex *ex, _ sym) {
    bytes *b = CAST(bytes, sym);
    return _ex_make_symbol(EX, b->bytes);
}
_ ex_is_string_equal(ex *ex, _ s1, _ s2) {
    return strcmp(CAST(cstring, s1), CAST(cstring, s2)) ? FALSE : TRUE;
}

_ ex_bytes_length(ex *ex, _ ob) {
    return integer_to_object(CAST(bytes, ob)->size);
}
/* This works on anything that has a 'dump' method defined. */
_ ex_write_bytes(ex *ex, _ ob_bytes, _ ob_port) {
    port *p = CAST(port, ob_port);
    leaf_object *l = ex->object_to_leaf(ex, ob_bytes);
    leaf_class *t;
    if (l && (t = leaf_type(l)) && t->dump) {
        t->dump(l, p);
        return VOID;
    }
    return TYPE_ERROR(ob_bytes);
}
_ ex_bytes_copy(ex *ex, _ ob) {
    return _ex_leaf_to_object(ex, bytes_copy(CAST(bytes, ob)));
}

/* Ports */
_ _ex_make_bytes_port(ex *ex, bytes *b) {
    return _ex_leaf_to_object(ex, (leaf_object *)port_bytes_new(b));
}
_ ex_port_name(ex *ex, _ ob) {
    port *p = CAST(port, ob);
    return _ex_make_string(ex, p->name);
}
_ ex_delete_file(ex *ex, _ ob) {
    return (-1 == remove(CAST(cstring, ob))) ? FALSE : TRUE;
}
_ ex_open_input_string(ex *ex, _ ob_str) {
    bytes *b = CAST(bytes, ob_str);
    bytes *copy_b = bytes_copy(b);
    return _ex_make_bytes_port(ex, copy_b);
}
_ ex_open_output_string(ex *ex) {
    bytes *b = bytes_new(20, 20);
    b->size = 0;
    return _ex_make_bytes_port(ex, b);
}
_ ex_get_output_string(ex *ex, _ ob_port) {
    port *p = CAST(port, ob_port);
    bytes *b = port_get_bytes(p);
    if (!b) return TYPE_ERROR(ob_port);
    return _ex_leaf_to_object(ex, b);
}

// Manually call finalizer, creating a defunct object.
_ ex_bang_finalize(ex *ex, _ ob) {
    aref *r = CAST(aref, ob);
    fin finalize = *(object_to_fin(EX, r->fin));
    finalize(r->object, ex);
    r->fin = VOID;
    r->object = VOID;
    return VOID;
}

_ ex_close_port(ex *ex, _ ob) {
    if (!object_to_port(ex, ob)) TYPE_ERROR(ob);
    return ex_bang_finalize(ex, ob);
}
_ ex_flush_output_port(ex *ex, _ ob) {
    port_flush(CAST(port, ob));
    return VOID;
}



/* Arithmetic */
_ ex_add1(ex *ex, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i + 1);
}
_ ex_sub1(ex *ex, _ o) {
    long i = CAST_INTEGER(o);
    return integer_to_object(i - 1);
}

/* Automatically convert. */
#define IS_INT(o) (GC_INTEGER == GC_TAG(o))
enum binop_tag {ADD,SUB,MUL,EQ,GT,LT};
#define DO_BINOP(op,a,b,z,zb) switch(op) {              \
    case ADD: z = a + b; break;                         \
    case SUB: z = a - b; break;                         \
    case MUL: z = a * b; break;                         \
    case EQ:  z = (a == b); zb = 1; break;              \
    case GT:  z = (a > b); zb = 1; break;               \
    case LT:  z = (a < b); zb = 1; break;               \
    default: ERROR("binop", integer_to_object(op)); }

double object_to_double(ex *ex, _ ob) {
    inexact *x = object_to_inexact(ex, ob);
    if (x) return x->value;
    if (IS_INT(ob)) return (double)(object_to_integer(ex, ob));
    return 0.0;
}

_ _ex_binop(ex *ex, _ a, _ b, enum binop_tag op) {
    int zb = 0;
    int is_int_a = IS_INT(a);
    int is_int_b = IS_INT(b);
    if (is_int_a && is_int_b) {
        int ia = object_to_integer(ex, a);
        int ib = object_to_integer(ex, b);
        int iz = 0;
        DO_BINOP(op,ia,ib,iz,zb);
        if (zb) return (iz == 0) ? FALSE : TRUE;
        else return integer_to_object(iz);
    }
    // assume the only other number kind are inexact (double float)
    double da = object_to_double(ex, a);
    double db = object_to_double(ex, b);
    double dz = 0;
    DO_BINOP(op,da,db,dz,zb);
    if (zb) return (dz == 0.0) ? FALSE : TRUE;
    else return _ex_leaf_to_object(ex, (leaf_object*)inexact_new(dz));
}

_ ex_add(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, ADD); }
_ ex_sub(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, SUB); }
_ ex_mul(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, MUL); }


/* This isn't completely standard since there are no rationals, but I
   prefer to get a consistent return type based on input types (as
   opposed to input values). */
_ ex_div(ex *ex, _ a, _ b) {
    double rv = object_to_double(ex, a) / object_to_double(ex, b);
    return _ex_leaf_to_object(ex, (leaf_object*)inexact_new(rv));
}
_ ex_quotient(ex *ex, _ a, _ b) {
    int ia = CAST_INTEGER(a);
    int ib = CAST_INTEGER(b);
    if (ib == 0) return ex_raise_type_error(ex, b);
    return integer_to_object(ia / ib);
}
_ ex_remainder(ex *ex, _ a, _ b) {
    int ia = CAST_INTEGER(a);
    int ib = CAST_INTEGER(b);
    if (ib == 0) return ex_raise_type_error(ex, b);
    return integer_to_object(ia % ib);
}

_ ex_eq(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, EQ); }
_ ex_gt(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, GT); }
_ ex_lt(ex *ex, _ a, _ b) { return _ex_binop(ex, a, b, LT); }

enum unop_tag {SIN,COS,TAN, ASIN,ACOS,ATAN, EXP,LOG, SQRT};
_ _ex_unop(ex *ex, _ a, enum unop_tag op) {
    inexact *inexact_a = object_to_inexact(ex, a);
    double da = inexact_a ? inexact_a->value :
               (IS_INT(a) ? (double)(object_to_integer(ex, a)) :
                ERROR("type", a));
    double dz = 0;
    switch (op) {
    case SIN: dz = sin(da); break;
    case COS: dz = cos(da); break;
    case TAN: dz = tan(da); break;

    case ASIN: dz = asin(da); break;
    case ACOS: dz = acos(da); break;
    case ATAN: dz = atan(da); break;

    case EXP: dz = exp(da); break;
    case LOG: dz = log(da); break;
    case SQRT: dz = sqrt(da); break;
    default: ERROR("unop", integer_to_object(op));
    }
    return _ex_leaf_to_object(ex, (leaf_object*)(inexact_new(dz)));
}

/* Inexact numbers */
_ ex_sin(ex *ex, _ a) { return _ex_unop(ex, a, SIN); }
_ ex_cos(ex *ex, _ a) { return _ex_unop(ex, a, COS); }
_ ex_tan(ex *ex, _ a) { return _ex_unop(ex, a, TAN); }

_ ex_asin(ex *ex, _ a) { return _ex_unop(ex, a, ASIN); }
_ ex_acos(ex *ex, _ a) { return _ex_unop(ex, a, ACOS); }
_ ex_atan(ex *ex, _ a) { return _ex_unop(ex, a, ATAN); }

_ ex_exp(ex *ex, _ a) { return _ex_unop(ex, a, EXP); }
_ ex_log(ex *ex, _ a) { return _ex_unop(ex, a, LOG); }
_ ex_sqrt(ex *ex, _ a) { return _ex_unop(ex, a, SQRT); }


_ ex_random(ex *ex) { return integer_to_object(random()); }



/* Strings / bytes */

_ ex_bytes_vector_append(ex *ex, _ ob) {
    vector *v = CAST(vector, ob);
    int len = vector_size(v), total = 0, i;
    bytes *b[len];
    for (i=0; i<len; i++) {
        b[i] = CAST(bytes, v->slot[i]);
        total += b[i]->size;
    }
    bytes *out = bytes_buffer_new(1+total);
    for (i=0; i<len; i++) {
        memcpy(bytes_allot(out, b[i]->size), b[i]->bytes, b[i]->size);
    }
    return _ex_leaf_to_object(ex, (leaf_object*)out);
}


/* Characters.  Note that currently characters and fixnums are the same. */
_ ex_char_upcase(ex *ex, _ ob) {  return integer_to_object(toupper(CAST_INTEGER(ob))); }
_ ex_char_downcase(ex *ex, _ ob) {  return integer_to_object(tolower(CAST_INTEGER(ob))); }


/* Lists and vectors. */
_ ex_make_vector_with_init(ex *ex, _ slots, _ init) {
    ENABLE_RESTART();
    long i,n = CAST_INTEGER(slots);
    vector *v = gc_alloc(ex->gc, n);
    if (!v) ERROR("invalid-size", slots);
    for(i=0; i<n; i++) v->slot[i] = init;
    return vector_to_object(v);
}

_ ex_reverse(ex *ex, _ lst) {
    ENABLE_RESTART();
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
        // FIXME: use poly predicate
        pair *p = object_to_lpair(ex, lst); // polymorphic
        if (!p) p = object_to_lnext(ex, lst);
        if (!p) p = object_to_ldata(ex, lst);
        if (!p) p = CAST(pair, lst);
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
_ ex_bang_append(ex *ex, _ a, _ b) {
    return ex_bang_reverse_append(ex, ex_bang_reverse(ex, a), b);
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
        pair *p = object_to_pair(ex, lst);
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
_ ex_lcons(ex *ex, _ car, _ cdr) {
    _ rv = ex_cons(ex, car, cdr);
    vector_set_flags(object_to_vector(ex, rv), TAG_LPAIR);
    return rv;
}

_ ex_car(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->car; }
_ ex_cdr(ex *ex, _ o)  { pair *p = CAST(pair, o); return p->cdr; }
_ ex_cadr(ex *ex, _ o) { pair *p = CAST(pair, ex_cdr(ex, o)); return p->car; }
_ ex_cdar(ex *ex, _ o) { pair *p = CAST(pair, ex_car(ex, o)); return p->cdr; }
_ ex_caar(ex *ex, _ o) { pair *p = CAST(pair, ex_car(ex, o)); return p->car; }
_ ex_cddr(ex *ex, _ o) { pair *p = CAST(pair, ex_cdr(ex, o)); return p->cdr; }
_ ex_caddr(ex *ex, _ o) { pair *p = CAST(pair, ex_cddr(ex, o)); return p->car; }


_ ex_lcar(ex *ex, _ o)  { pair *p = CAST(lpair, o); return p->car; }
_ ex_lcdr(ex *ex, _ o)  { pair *p = CAST(lpair, o); return p->cdr; }


_ ex_bang_set_car(ex *ex, _ op, _ o) {
    pair *p = CAST(pair, op); p->car = o; return VOID;
}
_ ex_bang_set_cdr(ex *ex, _ op, _ o) {
    pair *p = CAST(pair, op); p->cdr = o; return VOID;
}

_ ex_find_slot_safe(ex *ex, _ E, _ var) {
    if (TRUE == ex_is_null(ex, E)) return FALSE;
    _ slot = CAR(E);
    _ name = CAR(slot);
    if (name == var) return slot;
    else return ex_find_slot(ex, CDR(E), var);
}

/* This leads to a 2.5 x speedup, but is unsafe if environment is
   replaced by a different data structure. */
_ ex_find_slot(ex *ex, _ E, _ var) {
    for(;;) {
        if (NIL == E) return FALSE;
        _ slot = _CAR(E);
        _ name = _CAR(slot);
        if (name == var) return slot;
        E = _CDR(E);
    }
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

// FIXME: http://people.csail.mit.edu/jaffer/r4rs_8.html#SEC44
// somewhat arbitrary
_ ex_is_eqv(ex *ex, _ a, _ b) {
    if (a == b) return TRUE;
    return FALSE;
}
// FIXME
// recursive comparison of pairs, vectors and strings
_ ex_is_equal(ex *ex, _ a, _ b) {
  again:
    if (a == b) return TRUE;
    if ((TRUE == IS_PAIR(a)) && (TRUE == IS_PAIR(b))) {
        if (FALSE == IS_EQUAL(CAR(a), CAR(b))) return FALSE;
        a = CDR(a);
        b = CDR(b);
        goto again;
    }
// FIXME: string wrapping sucks: needs to be done in EX.
#if 0
    bytes *ba, *bb;
    if ((ba = object_to_bytes(a, ex)) && (bb = object_to_bytes(b, ex))) {
        int i;
        if (ba->size != bb->size) return FALSE;
        for (i=0; i<ba->size; i++) {
            if (ba->bytes[i] != bb->bytes[i]) return FALSE
        }
        return TRUE;
    }
#endif
    // FIXME: vectors
    return FALSE;
}

_ ex_is_list(ex *ex, _ o) {
    _ head = o;
  again:
    if(TRUE==IS_NULL(o)) return TRUE;
    if(FALSE==IS_PAIR(o)) return FALSE;
    o = CDR(o);
    if (o == head) return FALSE; // infinite list
    goto again;
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

_ ex_vector_to_list(ex *ex, _ vec) {
    ENABLE_RESTART();
    vector *v = CAST(vector, vec);
    int i,n = vector_size(v);
    _ lst = NIL;
    for(i=n-1; i>=0; i--) { lst = CONS(v->slot[i], lst); }
    return lst;
}

_ ex_list_clone(ex *ex, _ lst) {
    ENABLE_RESTART();
    if (NIL == lst) return lst;
    _ res = CONS(VOID, NIL);
    pair *in,*out;
    out = object_to_pair(ex, res);
    for(;;) {
        in  = CAST(pair, lst);
        if (NIL == in->cdr) return res;
        out->cdr = CONS(VOID,NIL);
        out = object_to_pair(ex, out->cdr);
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

static void _ex_print_stack(ex *ex, _ ob, int n) {
    if (NIL == ob) {
        _ex_printf(ex, "<%d>", n);
        return;
    }
    else {
        pair *p = object_to_lpair(ex, ob);
        if (!p) p = CAST(pair, ob);
        _ex_print_stack(ex, _CDR(ob), n+1);
        _ex_printf(ex, " ");
        ex->write(ex, _CAR(ob));
    }
}
_ ex_post_stack(ex *ex, _ ob) {
    _ex_print_stack(ex, ob, 0);
    _ex_printf(ex, "\n");
    return VOID;
}



/* ERRORS */

_ ex_trap(ex *ex) {
#ifdef POSIX
    kill(getpid(), SIGTRAP);
#endif
    return VOID;
}


_ ex_fatal_print_error(ex *ex, _ tag_o, _ arg_o) {
    _ex_printf(ex, "FATAL ERROR: ");
    ex->write(ex, tag_o); _ex_printf(ex, ": ");
    ex->write(ex, arg_o); _ex_printf(ex, "\n");
    TRAP();
    exit(1);
}
_ _ex_interrupt(ex *ex, int rv, ex_error_info *info) {
    if (ex->l.set_error && likely((!ex->fatal))) {
        leaf_raise(&ex->l, rv, info);
        return VOID;
    }
    if (!ex->fatal) _ex_printf(ex, "(out-of-context) ");
    return ex_fatal_print_error(ex, info->tag, info->arg);
}
_ ex_raise_error(ex *ex,  _ tag_o, _ arg_o) {
    ex_error_info info = {tag_o, arg_o};
    return _ex_interrupt(ex, EXCEPT_ABORT, &info);
}
_ ex_halt_vm(ex *ex, _ value) {
    return _ex_interrupt(ex, EXCEPT_HALT, NULL);
}
_ ex_raise_type_error(ex *ex, _ arg_o) {
    return ex_raise_error(ex, SYMBOL("type"), arg_o);
}
_ ex_raise_nargs_error(ex *ex, _ arg_o) {
    return ex_raise_error(ex, SYMBOL("nargs"), arg_o);
}
_ _ex_restart(ex *ex) {
    return _ex_interrupt(ex, EXCEPT_RESTART, NULL);
}




/* IO */

/* Use lowelevel port access to be independent of object wrapping. */

_ _ex_boot_port(ex *ex, port *bootport) {
    _ expr      = _ex_read(ex, bootport);
    _ junk_expr = _ex_read(ex, bootport);
    if (EOF_OBJECT != junk_expr) {
        fprintf(stderr, "Junk in boot file:\n");
        _ex_write(ex, junk_expr);
    }
    port_free(bootport);
    return expr;
}
_ _ex_boot_file(ex *ex,  struct ex_bootinfo *boot) {
    const char *bootfile = boot->source;
    port *bootport = port_file_new(fopen(bootfile, "r"), bootfile);
    if (!bootport) {
        fprintf(stderr, "Can't load boot file: %s\n", bootfile);
        return ex_raise_error(ex, SYMBOL("boot"), VOID);
    }
    return _ex_boot_port(ex, bootport);
}
_ _ex_boot_string(ex *ex,  struct ex_bootinfo *boot) {
    if (!boot->size) boot->size = strlen(boot->source);
    port *bootport = port_bytes_new(bytes_const_new(boot->source, boot->size));
    return _ex_boot_port(ex, bootport);
}
