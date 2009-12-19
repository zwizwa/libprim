/*

   PF: Compiler and helper functions.

   The code in this file is partitioned in two classes:

   px_   non-linear expression primitives
   _px_  misc functions (not respecting px_ nor pf_ API) 

   Note that the px_ functions are *NOT LINEAR*.  They respect the API
   of the EX language: a mostly functional dynamically typed
   expression language using the C-stack.  The linear stack operations
   carry the pf_ prefix and are defined separately in the pf.c file.
   The compiler is written in terms of nonlinear functions because:

      - The code graph it produces is nonlinear (loops + procedure reuse)

      - C's lexical scope meshes better with an expression language
        than a stack language.

      - It will make the interface with SC simpler, which is also
        written in EX.

      - I find writing a compiler (a tree/graph processor) in a
        combinator language needs too much cleverness.  Due to
        reliance on random access, data structure translation begs for
        lexical scope, and is facilitated by pattern matching.
 */

#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#include <pf/pf.h>
#include <pf/px.h>

/* Leaf object wrappers. */
DEF_RC_TYPE(port)
DEF_RC_TYPE(bytes)
DEF_RC_TYPE(inexact)
DEF_RC_TYPE(channel)


/* TOOLS+DATA */
_ px_abort(pf *pf, _ tag, _ arg) {
    return RAISE_ERROR(tag, arg);
}
_ _px_top(pf *pf) {
    if (unlikely (NIL == pf->p)) px_error_underflow(pf);
    return _CAR(pf->p);
}
_ _px_second(pf *pf) {
    if (unlikely (NIL == pf->p)) px_error_underflow(pf);
    _ more = _CDR(pf->p);
    if (unlikely (NIL == more)) px_error_underflow(pf);
    return _CAR(more);
}

_ px_error_underflow(pf *pf) {
    return px_abort(pf, pf->s_underflow, VOID);
}

_ _px_make_rc(pf *pf, leaf_object *ob) {
    rc *rc = malloc(sizeof(*rc));
    leaf_init(&rc->base, rc_type());
    rc->free = (rc_free)leaf_free;
    rc->ctx = ob;
    rc->rc = 1;
    return const_to_object(rc);
}
leaf_object *_px_object_to_leaf(pf *pf, object ob) {
    rc *x = object_to_rc(ob);
    if (!x) return NULL;
    return x->ctx;
}
_ _px_make_port(pf *pf, FILE *f, const char *name) {
    return _px_make_rc(pf, (leaf_object*)port_file_new(stdout, name));
}
_ _px_make_string(pf *pf, const char *name) {
    return _px_make_rc(pf, (leaf_object*)bytes_from_cstring(name));
}
_ _px_make_qstring(pf *pf, const char *name) {
    return _px_make_rc(pf, (leaf_object*)bytes_from_qcstring(name));
}
_ _px_make_symbol(pf *pf, const char *str){
    return const_to_object(symbol_from_cstring(str));
}

_ px_display(pf *pf, _ ob) { 
    bytes *b = CAST(bytes, ob);
    port_write(_px_port(pf), b->bytes, strlen(b->bytes));
    return VOID;
}


/* MEMORY MANAGEMENT

   The stack machine's inner data model consists of constants
   (permanent data), refcount managed abstact leaf objects and a
   linear tree of cons cells.

   The outer memory is a classic GC managed graph.  RC-managed data
   traversing the linear->graph boundary needs to be properly wrapped
   to synchronize the two memory managers.

   Graph data can be treated as constants in the linear memory, as the
   linear memory tree is part of the GC roots.

   Implementation: Linear lists are tagged differently from ordinary
   cons cells.  However, the *unsafe* macros _CAR _CDR ... can still
   access them, since their layout is the same.  They should always be
   constructed through LINEAR_CONS, not LCONS.  The former uses the
   freelist, while the latter allocates a cell from GC memory.

 */

/* Allocate/reuse cell. */

_ px_linear_cons(pf *pf, _ car, _ cdr) {
    _px_need_free(pf);
    _ rv = pf->freelist;
    pf->freelist = _CDR(pf->freelist);
    _CAR(rv) = car;
    _CDR(rv) = cdr;
    return rv;
}
_ px_linear_next(pf *pf, _ car, _ cdr) {
    _ ob = px_linear_cons(pf, car, cdr);
    vector *v = object_to_vector(ob);
    vector_reset_flags(v, TAG_LNEXT);
    return ob;
}
_ px_linear_data(pf *pf, _ car, _ cdr) {
    _ ob = px_linear_cons(pf, car, cdr);
    vector *v = object_to_vector(ob);
    vector_reset_flags(v, TAG_LDATA);
    return ob;
}


/* Unlink will RC manage objects, and move pairs to the freelist. */
static _ _px_unlink_pop(pf *pf, _ lst) {
    _ ob = MOVE(_CAR(lst), VOID);
    _px_unlink(pf, ob);
    _px_to_free(pf, &lst);
    return lst;
}
static void _rc_unlink(rc *x) {
    if (!(x->rc--)) {
        x->free(x->ctx);
        free(x);
    }
}
void _px_unlink(pf* pf, _ ob) {
    rc *x;
  again:
    /* RC objects: dec RC and possibly free */
    if ((x = object_to_rc(ob))) {
        _rc_unlink(x);
    }
    /* Lists: recurse. */
    else if (object_to_lpair(ob) ||
             object_to_ldata(ob) ||
             object_to_lnext(ob)) {
        ob = _px_unlink_pop(pf, ob);
        goto again;
    }
}
/* Link will RC++ objects and recursively copy pair structures, using
   pairs from the freelist.. */
_ _px_link(pf *pf, _ ob) {
    rc *x = object_to_rc(ob);
    pair *p;
    if (x) { 
        x->rc++;
        return ob;
    }
    else if ((p = object_to_lpair(ob))) {
        return LINEAR_CONS(_px_link(pf, p->car),
                           _px_link(pf, p->cdr));
    }
    else if ((p = object_to_lnext(ob))) {
        return LINEAR_NEXT(_px_link(pf, p->car),
                           _px_link(pf, p->cdr));
    }
    else if ((p = object_to_ldata(ob))) {
        return LINEAR_DATA(_px_link(pf, p->car),
                           _px_link(pf, p->cdr));
    }
    else return ob;
}
/* Whenever data is exported to the GC-managed side (graph memory or
   outer memory), CONS cells are copied and RC structs are wrapped.
   This performs a _copy_ instead of a an in-place move, which
   would interact badly with GC aborts due to mutation.

   There are 2 kinds of data structures hold linear values: a mutable
   box, and an immutable value.  BOX values are treated as graph
   values (constants), while LIN values are always unpacked into
   linear memory. */
static void _gc_unlink(_ ob, pf *pf) { _px_unlink(pf, ob); }
static void *unlink_fin = _gc_unlink;
_ px_box(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_BOX, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
_ px_lin(pf *pf, _ ob) {
    return gc_make_tagged(GC, TAG_LIN, 2, 
                          fin_to_object((void*)(&unlink_fin)), ob);
}
_ px_copy_to_graph(pf *pf, _ ob) {
    rc *x;
    pair *p;
    /* Wrap all RC objects in a LIN struct */
    if ((x = object_to_rc(ob))) {
        x->rc++;
        return LIN(ob);
    }
    /* Recursively copy the tree. */
    else if ((p = object_to_lpair(ob))) {
    
        return CONS(COPY_TO_GRAPH(p->car),
                    COPY_TO_GRAPH(p->cdr));
    }
    /* FIXME: Seq code contination frames are currently not
       representable outside the VM. */
    else if ((p = object_to_lnext(ob)) ||
             (p = object_to_ldata(ob))) {
        _ex_printf(EX, "WARNING: lnext/ldata -> nonlinear pair conversion.\n");
        return CONS(COPY_TO_GRAPH(p->car),
                    COPY_TO_GRAPH(p->cdr));
    }
    else return ob;
}
_ px_copy_from_graph(pf *pf, _ ob) {
    pair *p;
    lin *l;
    /* Unwrap LIN objects. */
    if ((l = object_to_lin(ob))) { 
        return _px_link(pf, l->object);
    }
    /* Recursive copy. */
    else if ((p = object_to_pair(ob))) {
        return LINEAR_CONS(COPY_FROM_GRAPH(p->car),
                           COPY_FROM_GRAPH(p->cdr));
    }
    else return ob;
}




/* EXPRESSIONS 

   It's simpler to factor out primitives as N -> 1 expressions, and
   then couple them (automatically?) to the parameter stack.  This
   uses the same naming convention as sc_ and ex_, namely

   px_  :  N x object -> object
   _px_ :  any other operation on *pf


*/


port *_px_port(pf *pf) {
    return object_to_port(pf->output);
}

/* Code _printing_. tries to guess what form of quotation the object
   came from.  The code is largely trial and error, trying to handle
   all the corner cases.  See also the quote, seq and prim cases in
   px_write() */

/* Print as part of code sequence. */
_ px_write_name_or_quotation(pf *pf, _ ob) {
    quote *q;
    lin *l;
    /* Try to resolve the name. */
    _ sym = UNFIND(pf->dict, ob);

    /* Print name if it has one. */
    if (FALSE != sym) return px_write(pf, sym);

    /* If the object was a data quotation that's not registered as
       code in the dictionary, print it as an inline quotation.*/
    if ((q = object_to_quote(ob))) {
        if ((object_to_seq(q->object)) ||
            (object_to_quote(q->object)) ||
            (object_to_prim(q->object))) {
            return px_write(pf, q->object);
        } else {
            _ex_printf(EX, "'");
            /* Don't print the LIN wrapper. */
            if ((l = object_to_lin(q->object))) {
                return px_write(pf, l->object);
            }
            else {
                return px_write(pf, q->object);
            }
        }
    }

    /* Otherwise indicate that there the object has no name. */
    return _ex_printf(EX, "_");
}

const char *CL = "[";
const char *CR = "]";

_ px_write(pf *pf, _ ob) {
    void *x;
    if ((x = object_to_box(ob))) {
        return _ex_write_vector(EX, "box", object_to_vector(ob));
    }
    else if ((x = object_to_lin(ob))) {
        return _ex_write_vector(EX, "lin", object_to_vector(ob));
    }
    /* SEQ and QUOTE are decompiled.  Use square brackets to
       distinguish from lists. */
    else if ((x = object_to_seq(ob))) {
        long max = 10;
        _ex_printf(EX, CL);
        for(;;) {
            seq *s = (seq*)x;
            px_write_name_or_quotation(pf, s->now);
            _ex_printf(EX, " ");
            ob = s->next;
            if (!(x = object_to_seq(ob))) {
                px_write_name_or_quotation(pf, s->next);
                _ex_printf(EX, CR);
                return VOID;
            }
            /* If the tail has a name, print that instead. */
            _ sym = UNFIND(pf->dict, ob);
            if (FALSE != sym) {
                px_write(pf, sym);
                return _ex_printf(EX, CR); 
            }
            /* Prevent loops from generating too much output. */
            if (!(--max)) {
                _ex_printf(EX, "..."); 
                return _ex_printf(EX, CR); 
            }
        }
    }
    /* Primitive or quoted datum. */
    else if ((x = object_to_quote(ob)) ||
             (x = object_to_prim(ob))) {
        quote *q = (quote*)x;
        /* Print it as a singleton. */
        _ex_printf(EX, CL);
        px_write_name_or_quotation(pf, ob);
        return _ex_printf(EX, CR);
    }
    else if (HALT == ob) {
        return _ex_printf(EX, "#halt");
    }
    else if (pf->ip_prompt_tag == ob) {
        return _ex_printf(EX, "#prompt");
    }
    return _ex_write(EX, ob);
}


/* COMPILER */

/* Compilation is factored into several steps.  The main distinction
   is again to perform the global side effects (update of the toplevel
   environement) _after_ all allocation has finished.

   These are the passes:

     1. Create skeleton environment.

     2. Translate s-expr -> SEQ | PRIM | QUOTE

     3. Resolve (remaining) undefined references.

     4. Patch toplevel environment.
*/


/* Convert definition list of (name . src) symbolic code pairs to a
   compiled dictionary of (name . code) pairs, using E_top for
   undefined references. */

_ px_skeleton_entry(pf *pf, _ code) { return CONS(CAR(code), VOID); }
_ px_compile_defs(pf *pf, _ E_top, _ defs) {
    /* Create skeleton dictionary. */
    _ E_local = _ex_map1_prim(EX, (ex_1)px_skeleton_entry, defs);
    _ penv  = E_local;
    _ pdefs = defs;
    /* Translate to code graph. */
    while (NIL != penv) { 
        _ entry = CAR(penv);
        _ src   = CDAR(pdefs);
        _CDR(entry) = px_compile_program_env(pf, E_top, E_local, src);
        penv  = CDR(penv);
        pdefs = CDR(pdefs);
    }
    /* Resolve all references. */
    px_bang_resolve(pf, E_top, E_local);

    // FIXME:
    /* Check degenerate loops */
    /* Snap pointers. */
    return E_local;
}

/* Compile anonymous symbolic code to code graph. */
_ px_quote(pf *pf, _ data)       { STRUCT(TAG_QUOTE, 1, data); }
_ px_seq(pf *pf, _ sub, _ next)  { STRUCT(TAG_SEQ, 2, sub, next); }


/* This one is insidious..  The input code is always nonlinear, but
   what if it already contains wrapped linear data?  This needs a
   properly specified quote/unquote semantics.

   Essentially, this can only receive atoms that can appear in *source
   code*, so we only translate nonlinear lists to linear ones.
*/

_ px_quote_source_datum(pf *pf, _ datum) {
    if ((object_to_pair(datum))) {
        return QUOTE(LIN(COPY_FROM_GRAPH(datum)));
    }
    else {
        return QUOTE(datum);
    }
}
_ px_compile_program_env(pf *pf, _ E_top, _ E_local, _ src) {
    _ rv;
    _ *cursor = &rv;
    if (NIL == src) return pf->ip_nop;
    /* Compile with proper tail calls. */
    for(;;) {
        _ compiled, datum = CAR(src);
        /* Quoted empty program. */
        if (NIL == datum) {
            compiled = QUOTE(pf->ip_nop);
        }
        else if (TRUE == IS_LIST(datum)) {
            /* Special Form. */
            _ tag = _CAR(datum);
            if (tag == pf->s_quote) {
                compiled = QUOTE_SOURCE_DATUM(_CADR(datum));
            }
            else if (tag == pf->s_var) {
                _ val = (NIL == _CDR(datum) ? VOID : _CADR(datum));
                compiled = QUOTE(BOX(COPY_FROM_GRAPH(val)));
            }
            /* Quoted subprogram. */
            else {
                compiled = QUOTE  // Doesn't need wrapping.
                    (COMPILE_PROGRAM_ENV(E_top, E_local, datum));
            }
        }
        /* If possible, dereference.  In case we're compiling
           non-recursive code, this first pass will produce fully
           linked code. */
        else if (TRUE == IS_SYMBOL(datum)) {
            _ val = FIND2(E_local, E_top, datum);
            if (FALSE == val) ERROR("undefined", datum);
            if (VOID != val) compiled = val;
            else compiled = datum;
        }
        /* Quote literal data. */
        else {
            compiled = QUOTE_SOURCE_DATUM(datum);
        }
        /* Return if this was the last one. */
        if (NIL == CDR(src)) {
            *cursor = compiled;
            return rv;
        }
        /* Allocate sequence if there's more to come. */
        else {
            _ seq = SEQ(compiled, VOID);
            *cursor = seq;
            cursor = &(object_to_seq(seq)->next);
            src = CDR(src);
        }
    }
}
_ px_compile_program(pf *pf, _ src) {
    return px_compile_program_env(pf, NIL, pf->dict, src);
}
_ px_make_loop(pf *pf, _ code) {
    _ ob = SEQ(code, VOID);
    object_to_seq(ob)->next = ob;
    return ob;
}

/* Walk the code, eliminating symbolic references where possible.
   Note: this only traverses the part of the code graph that's the
   result of a compilation. */
_ _px_resolve_sub(pf *pf, _ E_top, _ E_local,  _ *cursor);
_ _px_resolve_non_seq(pf *pf, _ E_top, _ E_local, _ *cursor) {
    _ sub = *cursor;
    quote *q;
    if ((q = object_to_quote(sub))) {
        return _px_resolve_sub(pf, E_top, E_local, &(q->object));
    }
    if (IS_SYMBOL(sub)) {
        _ val = FIND2(E_local, E_top, sub);
        if (FALSE != val) {
            *cursor = val;
            return ONE;
        }
    }
    return ZERO;
}
/* Resolve a flat subroutine without recursing into the call graph. */
_ _px_resolve_sub(pf *pf, _ E_top, _ E_local, _ *cursor) {
    _ derefs = ZERO;
    for(;;) {
        seq* s;
        _ sub = *cursor;
        /* Sequence, resolve NOW, then continue. */
        if ((s = object_to_seq(sub))) {
            _ dr = _px_resolve_non_seq(pf, E_top, E_local, &s->now);
            derefs = ADD(derefs, dr);
            cursor = &s->next;
        }
        /* Other: resolve and return. */
        else {
            _ dr = _px_resolve_non_seq(pf, E_top, E_local, cursor);
            return ADD(derefs, dr);
        }
    }
}
_ px_bang_resolve(pf *pf, _ E_top, _ E_local) {
    _ todo = E_local;
    _ derefs = ZERO;
    while (NIL != todo) {
        _ *cursor = &_CDAR(todo);
        derefs = ADD(derefs, _px_resolve_sub(pf, E_top, E_local, cursor));
        todo = CDR(todo);
    }
    return derefs;
}


_ px_define(pf *pf, _ defs) {
    /* Create an isolated environment. */
    _ E = px_compile_defs(pf, pf->dict, defs);

    /* Patch it into the global one. */
    while (NIL != E) {
        _ var = _CAAR(E);
        _ val = _CDAR(E);
        // _ex_printf(EX, "define: "); POST(var);
        pf->dict = ENV_DEF(pf->dict, var, val);
        E = _CDR(E);
    }
    // _PRINT_DICT();
    return VOID;
}


/* Linear code serves as a substrate for any form of code constructed
   linearly at run-time.  This includes continuations, partial
   applications and code compositions / concatenations.

   Currently linear code is represented by 2 tags: NEXT and DATA

*/

_ px_bang_linear_compose(pf *pf, _ partial, _ full) {
    _ *x = &partial;
    /* Wind to end. */
    while (object_to_ldata(*x) ||
           object_to_lnext(*x)) {
        x = &(_CDR(*x));
    }
    *x = full;
    return partial;
}





_ px_error_undefined(pf *pf, _ var) {
    return ERROR("undefined", var);
}



/* Converting data that is passed through the exception handlers to
   linear form needs some care.

   The following constraints are necessary:

   - All _linear_ data needs to be reachable from the machine state
     _at all times_.  C stack can only contain access pointers into
     this data structure.

   - Therefore, all _linear_ data that is referenced through the
     exception mechanism needs to be _linked_ because it is already
     residing somehwere else.

   - Nonlinear data that passes through the exception mechanism can
     safely be treated as a constant.

*/
_ px_linearize_exception(pf *pf, _ ob) {
    return _px_link(pf, ob);
}
