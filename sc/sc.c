
#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include <config.h>

#include <sc/sc.h>




/* --- PRIMITIVES --- */

/* To simplify the implementation, most C functions are implemented as
   Scheme primitives operating on Scheme values.  They use the prefix
   "sc_".

   Note in particular that interpreter data constructors are available
   in Scheme, and that "sc_eval_step()" is re-entrant with primitive
   errors limited to the innermost step.

   The functions operating on *sc that are too lowlevel to respect the
   "sc_" ABI (because they use values that cannot be represented as a
   Scheme object, or because they otherwize violate behavioural
   constraints) are prefixed "_sc".  These functions are kept to a
   minimum.
*/


/**** Code shared with new VM ****/


// GC finalized objects
// DEF_AREF_TYPE(ck)
DEF_AREF_TYPE(port)
DEF_AREF_TYPE(bytes)
DEF_AREF_TYPE(inexact)
DEF_AREF_TYPE(channel)


_ sc_make_aref(sc *sc, _ F, _ O)       {return STRUCT(TAG_AREF,    2, F,O);}
_ sc_make_error(sc *sc, _ P, _ T, _ A) {return STRUCT(TAG_ERROR,   3, P,T,A);}

_ sc_is_aref(sc *sc, _ o)        { return _is_vector_type(o, TAG_AREF); }
_ sc_is_error(sc *sc, _ o)       { return _is_vector_type(o, TAG_ERROR); }


/* Predicates for primitive objects are derived from their
   object_to_pointer cast: if it returns NULL, the type isn't
   correct. */
#define OBJECT_PREDICATE(cast) \
    {if (cast(o)) return TRUE; else return FALSE;}
// _ _DISABLED_sc_is_ck(sc *sc, _ o)     { OBJECT_PREDICATE(object_to_ck); }
_ sc_is_port(sc *sc, _ o)   { OBJECT_PREDICATE(object_to_port); }
_ sc_is_bytes(sc *sc, _ o)  { OBJECT_PREDICATE(object_to_bytes); }



/* Wrap a leaf object in an aref struct.  The destructor is gathered
   from the leaf_class.  Note that GC finalizers are pointers to
   function pointers (this is because function pointers themselves
   might not be aligned, and thus have no space for bit tags. */


_ _sc_make_aref(sc *sc, void *_x) {
    static fin leaf_free_ptr = (fin)leaf_free;
    leaf_object *x = _x;
    if (!x) ERROR("aref", VOID);
    leaf_class *t = leaf_type(x);
    fin *f = &leaf_free_ptr;
    return sc_make_aref(sc, fin_to_object(f), const_to_object(x));
}

_ sc_global(sc *sc, _ n) { 
    return VECTOR_REF(sc->global, n); 
}
_ sc_bang_set_global(sc *sc, _ n, _ val) { 
    return BANG_VECTOR_SET(sc->global, n, val); 
}


_ sc_bang_set_toplevel(sc *sc, _ val)       { _GLOBAL_SET(toplevel, val); }
_ sc_bang_set_toplevel_macro(sc *sc, _ val) { _GLOBAL_SET(toplevel_macro, val); }

_ sc_toplevel(sc *sc)       { _GLOBAL(toplevel); }
_ sc_toplevel_macro(sc *sc) { _GLOBAL(toplevel_macro); }


/*  Add to or mutate toplevel env. */
_ sc_bang_def_global(sc* sc, _ slot, _ var, _ val) {
    symbol *s;
    _ env = sc_global(sc, slot);
    if (!(s=object_to_symbol(var))) TYPE_ERROR(var);
    // _ex_printf(EX, "DEF %s: \n",s->name); // sc_write(EX, val);
    sc_bang_set_global(sc, slot, ENV_DEF(env, var, val));
    // return VOID;
    return val; // emacs-like behaviour - simpler when debugging
}
_ sc_bang_def_toplevel(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel, var, val);
}
_ sc_bang_def_toplevel_macro(sc* sc, _ var, _ val) {
    return sc_bang_def_global(sc, sc_slot_toplevel_macro, var, val);
}


// FIXME: should be parameter
port *_sc_port(sc *sc) {
    return object_to_port(CURRENT_ERROR_PORT());
}
_ sc_current_error_port(sc *sc)  { return sc_global(sc, sc_slot_error_port); }
_ sc_current_input_port(sc *sc)  { return sc_global(sc, sc_slot_input_port); }
_ sc_current_output_port(sc *sc) { return sc_global(sc, sc_slot_output_port); }

_ sc_write_port(sc *sc, _ o, _ o_port) {
    /* This `dynamic-wind' hack only works because we're sure there
       are no aborts in this dynamic extent!  FIXME: use explicit
       lexical variables where possible. */
    port *p = CAST(port, o_port);
    _ saved_port = sc_global(sc, sc_slot_error_port);
    sc_bang_set_global(sc, sc_slot_error_port, o_port);
    _ rv = sc->m.write(EX, o);
    sc_bang_set_global(sc, sc_slot_error_port, saved_port);
    return rv;
}



/* Unwrap leaf objects: it is assumed that all aref-wrapped objects
   are leaf objects.  See constructor. */
leaf_object *_sc_object_to_leaf(sc *sc, _ o) {
    aref *a = object_to_aref(o); if (!a) return NULL;
    leaf_object *x = object_to_const(a->object);
    return x;
}
/* This is used in channel communication: the leaf object's ownership
   is transferred to the other end. */
void _sc_object_erase_leaf(sc *sc, _ o) {
    aref *a = object_to_aref(o); if (!a) return;
    a->fin = a->object = const_to_object(NULL);
}


_ sc_print_error(sc *sc, _ err) {
    if (TRUE == sc_is_error(sc, err)) {
        error *e = object_to_error(err);
        _ex_printf(EX, "ERROR");
        if (TRUE == IS_PRIM(e->prim)) {
            prim *p = object_to_prim(e->prim);
            /* If the recorded primitive is sc_print_error itself,
               this means the error is a result of a direct
               invocation, i.e. a highlevel error. */
            if (SYMBOL("raise-error") != p->var) { 
                symbol *s = object_to_symbol(p->var);
                if (s) _ex_printf(EX, " in `%s'", s->name); 
            }
        }
        _ex_printf(EX, ": ");
        _ex_write(EX, e->tag); _ex_printf(EX, ": ");
        _ex_write(EX, e->arg); _ex_printf(EX, "\n");
        // sc_write_stderr(sc, e->tag); _ex_printf(EX, ": ");
        // sc_write_stderr(sc, e->arg); _ex_printf(EX, "\n");
    }
    return VOID;
}
_ sc_exit(sc *sc) {
    exit(0);
}

static void _sc_check_gc_size(sc *sc) {
    /* Check mem size. */
    gc *gc = EX->gc;
    long used = gc->current_index;
    long free = gc->slot_total - used;
    if (free < 100) {
        _ex_printf(EX, "growing GC\n");
        gc_alloc(gc, 100); // grow.
    }
}

void _sc_def_prims(sc *sc, prim_def *prims) {
    prim_def *prim;
    for (prim = prims; prim->name; prim++) {
        DEF(prim->name, prim->fn, prim->nargs);
    }
}


#define MARK(x) x = gc_mark(sc->m.gc, x)
void _sc_mark_roots(sc *sc, gc_finalize fin) {
    // ex_trap(EX);
    // printf("gc_mark()\n");
    // sc_post(sc, sc->state);
    if (EX->stateful_context) {
        _ex_printf(EX, "FATAL: GC triggered in stateful context.");
        ex_trap(EX);
    }
    MARK(sc->global);
    MARK(sc->error);
#ifdef SC_NEW_VM
    MARK(sc->c);
    MARK(sc->e);
    MARK(sc->k);
#endif

    if (fin) {
        /* We're given a finalizer continuation to aid us in aborting
           the C context that gave rise to the collection.  We use
           this to restart the current interpretation step saved in
           sc->state.  */
        fin(sc->m.gc);
        long used = sc->m.gc->current_index;
        long free = sc->m.gc->slot_total - used;
        GC_DEBUG { _ex_printf(EX, ";; gc %d:%d\n", (int)used, (int)free); }
        _ex_restart(EX);
    }
    else {
        /* No finalizer continuation means that this call is part of a
           gc_grow() operation, called from _sc_overflow(), which will
           handle restart.  We need to return to caller. */
        return;
    }
}
static _ _sc_make_prim(sc *sc, void *fn, long nargs, _ var) {
    prim *p = malloc(sizeof(*p));
    leaf_init(&p->base, prim_type());
    p->fn = fn;
    p->nargs = nargs;
    p->var = var;
    return const_to_object(p);
}
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs) {
    _ var = SYMBOL(str);
    sc_bang_def_toplevel(sc, var, _sc_make_prim(sc, fn, nargs, var));
}

_ sc_read_no_gc(sc *sc, _ o) {
    port *p = CAST(port, o);
    return _ex_read(EX, p);
}
_ sc_script_dir(sc *sc) {
    return STRING(PRIM_HOME);
}


/* Toplevel VM loop.  This function captures the GC restart and
   primitive error exceptions.

     - This function is NOT re-entrant.  

       Note that this would involve some C stack / Scheme continuation
       synchronization.  Currently this is not supported: use Scheme
       as the toplevel control.

     - It is allowed to use gc_alloc() outside this loop to create
       data (to pass to this function) as long as you can prove that
       there will be no collection.  Triggering GC outside of this
       function will invalidate previously allocated data (it will
       have moved).
*/

_ _sc_continue_dynamic(sc *sc, sc_loop _sc_loop, sc_abort _sc_abort) {

    if (sc->m.entries) {
        _ex_printf(EX, "WARNING: multiple _sc_top() entries.\n");
        return NIL;
    }
    pthread_mutex_lock(&EX->machine_lock);
    sc->m.entries++;
    for(;;) {

        switch(setjmp(sc->m.except)) {
        case EXCEPT_TRY:

            /* Pre-allocate the error struct before the step() is
               entered to prevent GC restarts when handling
               exceptions. */ 
            if (FALSE == sc->error) {
                sc->error = sc_make_error(sc, VOID, VOID, VOID);
            }
            /* Run the interpreter loop defined elsewhere. */
            _sc_loop(sc);

        case EXCEPT_HALT: {
            sc->m.entries--;
            pthread_mutex_unlock(&EX->machine_lock);
            return sc->m.error_arg;
        }

        case EXCEPT_ABORT: {
            error *e = object_to_error(sc->error);
            if (unlikely(NULL == e)) { TRAP(); }
            
            /* Wrap error info and clear the low level error state. */
            e->tag  = sc->m.error_tag;              sc->m.error_tag = NIL;
            e->arg  = sc->m.error_arg;              sc->m.error_arg = NIL;
            e->prim = const_to_object(sc->m.prim);  sc->m.prim = NULL;

            /* Run the error handler defined elsewhere. */
            _sc_abort(sc);
            
            /* Unlink error struct, so it won't get overwritten. */
            sc->error = FALSE;
        }

        case EXCEPT_GC:
            /* Continue with current state = restart step. */
            sc->m.prim = NULL;
        }
    }
}



static prim_def ex_prims[] = ex_table_init;
static prim_def sc_prims[] = sc_table_init;

#define SHIFT(n) {argv+=n;argc-=n;}
int _sc_init(sc *sc, int argc, const char **argv, sc_bootinfo *info) {

    memset(info, 0, sizeof(*info));
    info->args = NIL;
    sc->m.entries = 0;

    /* Read command line interpreter options options. */
    SHIFT(1); // skip program name
    while ((argc > 0) && ('-' == argv[0][0])) {
        if (!strcmp("--boot", argv[0])) { info->bootfile = argv[1]; SHIFT(2); }
        else if (!strcmp("--verbose", argv[0])) { SHIFT(1); info->verbose = 1; }
        else if (!strcmp("--fatal", argv[0])) { SHIFT(1); sc->m.fatal = 1; }
        else if (!strcmp("--eval", argv[0])) { info->evalstr = argv[1]; SHIFT(2); }
        else if (!strcmp("--", argv[0])) { SHIFT(1); break; }
        else {
            fprintf(stderr, "option `%s' not recognized\n", argv[0]);
            return 1;
        }
    }
    
    /* This is taken during resume() and freed during select() */
    pthread_mutex_init(&EX->machine_lock, NULL);

    /* Garbage collector. */
    sc->m.gc = gc_new(20000, sc, 
                      (gc_mark_roots)_sc_mark_roots,
                      (gc_overflow)_ex_overflow);

    /* Nb of cells guaranteed to be available to primitive. */
    EX->gc_guard_cells = 100;

                    

    /* EX virtual methods */
    sc->m.port = (_ex_m_port)_sc_port;
    sc->m.write = (ex_m_write)_ex_write; //;;sc_write_stderr;
    sc->m.make_pair = ex_cons;
    sc->m.leaf_to_object = (_ex_m_leaf_to_object)_sc_make_aref;
    sc->m.object_to_leaf = (_ex_m_object_to_leaf)_sc_object_to_leaf;
    sc->m.object_erase_leaf = (_ex_m_object_erase_leaf)_sc_object_erase_leaf;

    /* Data roots. */
    _ in  = _ex_make_file_port(EX, stdin,  "stdin");
    _ out = _ex_make_file_port(EX, stdout, "stdout");
    _ err = _ex_make_file_port(EX, stderr, "stderr");
    sc->global = gc_make_tagged(sc->m.gc, 
                                TAG_VECTOR,
                                7,
                                NIL,  // toplevel
                                NIL,  // macro
                                NIL,  // state
                                NIL,  // abort
                                in,
                                out,
                                err);
    sc->error = FALSE;

    /* Pass command line arguments to scheme. */
    while ((argc > 0)) { info->args = CONS(STRING(argv[0]), info->args); SHIFT(1); }
    info->args = BANG_REVERSE(info->args);
    sc_bang_def_toplevel(sc, SYMBOL("args"), info->args);

    /* Highlevel bootstrap. */
    if (!info->bootfile) info->bootfile = getenv("PRIM_BOOT_SCM");
    if (!info->bootfile) info->bootfile = PRIM_HOME "boot.scm";
    if (info->verbose) _ex_printf(EX, "SC: %s\n", info->bootfile);
    if (sc->m.fatal) _ex_printf(EX, "SC: all errors are fatal: SIGTRAP for bootstrap debugging.\n");

    /* Code entry point. */
    if (!info->evalstr) info->evalstr = "(repl)"; 

    /* Primitive defs. */
    _sc_def_prims(sc, ex_prims);
    _sc_def_prims(sc, sc_prims);

    return 0;
}
