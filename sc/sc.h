#ifndef _SC_H_
#define _SC_H_

typedef struct _scheme sc;


#define _GLOBAL(name) return sc_global(sc, sc_slot_##name)
#define _GLOBAL_SET(name, val) return sc_bang_set_global(sc, sc_slot_##name, val)


/* Global Scheme State*/
#define sc_slot_toplevel        integer_to_object(0)
#define sc_slot_toplevel_macro  integer_to_object(1)
#define sc_slot_state           integer_to_object(2)
#define sc_slot_abort_k         integer_to_object(3)
#define sc_slot_input_port      integer_to_object(4)
#define sc_slot_output_port     integer_to_object(5)
#define sc_slot_error_port      integer_to_object(6)


struct _scheme {
    /* Scheme extends the EX language's memory model, which uncludes
       leaf types and a garbage collector. */
    ex m;

#ifdef SC_NEW_VM
    /* Machine state */
    _ c;   // code 
    _ e;   // environment
    _ k;   // continuation
#endif

    /* Highlevel global state data is accessible from Scheme. */
    _ global;

    /* Struct to hold errors. */
    _ error;

    /* Lowlevel implementation data.  The object values below don't
       need to be marked because they are short-lived, or constant. */


    /* Async command I/O channels. */
    // leaf_object *async_in;
    // leaf_object *async_out;

};


// SUPER
#define EX (&sc->m)


/* The ck atoms have a free() finalizer, so need to be wrapped in an
   aref struct */
static inline void *object_aref_struct(object ob, void *type) {
    aref *ref;
    void *x;
    if ((ref = object_to_aref(ob)) &&
        (x = object_struct(ref->object, type))) return x;
    else return NULL;
}

/* Scheme specific object wrappers.  Note that these functions need to
   show up at link time, as they are part of the binary interface. */
#define DEF_AREF_TYPE(name) \
    name *object_to_##name(object ob) { \
        return (name*)object_aref_struct(ob,name##_type()); }




/* ROOT OBJECTS */
#define ROOT_ENV 0

#define NUMBER(n)     integer_to_object(n)
// #define ERROR(msg, o) sc_raise_error(sc, SYMBOL(msg), o)
// #define TYPE_ERROR(o) sc_raise_type_error(sc, o)

// renames
#define sc_make_pair sc_cons


// for geneterated bootstrap code
void _sc_def_prim(sc *sc, const char *str, void *fn, long nargs);
#define DEF(str,fn,nargs) _sc_def_prim (sc,str,fn,nargs)


_ _sc_make_aref(sc *sc, void *x);  // actually. x is a leaf_object
_ _sc_make_symbol(sc *sc, const char *str);
_ _sc_make_string(sc *sc, const char *str);

_ _sc_printf(sc *sc, char *fmt, ...);



port *_sc_port(sc *sc);


#define A(n) a[n] = CAR(args); args = _CDR(args)
static inline _ _sc_call(sc *sc, void *p, int nargs, _ args) {
    _ a[5];                     
          if (0 == nargs) return ((ex_0)p)(EX);
    A(0); if (1 == nargs) return ((ex_1)p)(EX, a[0]);    
    A(1); if (2 == nargs) return ((ex_2)p)(EX, a[0], a[1]);    
    A(2); if (3 == nargs) return ((ex_3)p)(EX, a[0], a[1], a[2]);    
    A(3); if (4 == nargs) return ((ex_4)p)(EX, a[0], a[1], a[2], a[3]);    
    A(4); if (5 == nargs) return ((ex_5)p)(EX, a[0], a[1], a[2], a[3], a[4]);    
    return ERROR("prim", integer_to_object(nargs));
}
#undef A


void _sc_mark_roots(sc *sc, gc_finalize fin);
leaf_object *_sc_object_to_leaf(sc *sc, _ o);
void _sc_object_erase_leaf(sc *sc, _ o);

typedef struct {
    const char *bootfile;
    const char *evalstr;
    int verbose;
    _ args;
} sc_bootinfo;

int _sc_init(sc *sc, int argc, const char **argv,
             sc_bootinfo *info);



#endif
