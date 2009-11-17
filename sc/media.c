/* libprim/media bindings for Scheme

   This file bridges the Scheme memory / control (exception) model
   with low-level C objects.  The function prototypes are mined to
   produce media.h_sc_prims contating media_table_init + macro defs.

   FIXME: 
     * this file should be autogenerated from the media/.h C objects. 
     * extend leaf objects with error handling mechanism
     * move some operations from scheme-specific to generic leaf.

 */

#include "scheme.h"
#include <media/ffmpeg.h>
#include <leaf/bytes.h>
#include "media.h_sc_prims"
#include <media/xwindow.h>
#include <media/xv.h>
#include <media/glx.h>
#include <leaf/grid.h>
#include <media/gsl.h>


/* Instead of storing the class object in the ex struct (see
   DEF_AREF_TYPE and ex's base_types member), it's also possible to
   store it in a global variable.  Here we use a _c postfix naming
   convention. */

#define DEF_TYPE(name) \
    static inline name *object_to_##name(object ob, ex *m) { \
        return (name*)object_aref_struct(ob,m,name##_type()); }

DEF_TYPE(codec)
DEF_TYPE(codec_context)
DEF_TYPE(vframe)
DEF_TYPE(aframe)

static prim_def media_prims[] = media_table_init;

void _sc_media_init(sc *sc) {
    av_register_all();
    _sc_def_prims(sc, media_prims);
}



/*** FFMPEG ***/

_ sc_make_codec(sc* sc, _ spec) {
    char *name = CAST(cstring, spec);
    codec *c = codec_new(name);
    if (!c) return ERROR("codec-not-found", spec);
    return _sc_make_aref(sc, (leaf_object*)codec_new(name));
}

_ sc_make_codec_context(sc *sc) {
    return _sc_make_aref(sc, (leaf_object *)codec_context_new());
}


_ sc_codec_context_info(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    codec_context_info(c, _sc_port(sc));
    return VOID;
}

_ sc_make_vframe(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    vframe *f = vframe_new(c);
    return _sc_make_aref(sc, (leaf_object *)f);
}

_ sc_make_aframe(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    aframe *f = aframe_new(c);
    if (!f) return INVALID(ob);
    return _sc_make_aref(sc, (leaf_object*)f);
}

_ sc_bang_frame_test(sc *sc, _ ob_frame, _ ob_ctx, _ ob_int) {
    int i = CAST_INTEGER(ob_int);
    vframe *f = CAST(vframe, ob_frame);
    codec_context *c = CAST(codec_context, ob_ctx);
    frame_test(f, c, i);
    return VOID;
}

/* As/dissociate codec context to codec. */
_ sc_codec_context_open(sc *sc, _ ctx, _ cod) {
    if (codec_context_open(CAST(codec_context, ctx), CAST(codec, cod)) < 0) {
        ERROR("invalid-context", cod);
    }
    return VOID;
}
_ sc_codec_context_close(sc *sc, _ ctx) {
    if (codec_context_close(CAST(codec_context, ctx)) < 0) {
        return ERROR("invalid-context", ctx);
    }
    return VOID;
}

_ sc_codec_to_string(sc *sc, _ ob) {
    codec *c = CAST(codec, ob);
    return _sc_make_string(sc, c->codec->name);
}

_ sc_codec_context_encode_video(sc *sc, _ ctx, _ frm, _ buf) {
    vframe *f = (frm == FALSE) ? NULL : CAST(vframe, frm); // delayed frames
    codec_context_encode_video(CAST(codec_context, ctx), f, CAST(bytes, buf));
    return VOID;
}

_ sc_codec_context_encode_audio(sc *sc, _ ctx, _ frm, _ buf) {
    aframe *f = (frm == FALSE) ? NULL : CAST(aframe, frm); // delayed frames
    codec_context_encode_audio(CAST(codec_context, ctx), f, CAST(bytes, buf));
    return VOID;
}


/*** X11 ***/

DEF_TYPE(xwindow)
DEF_TYPE(xdisplay)
DEF_TYPE(grid)
DEF_TYPE(grid_proc)

_ sc_make_display(sc *sc, _ ob) {
    return _sc_make_aref(sc, xdisplay_new(CAST(cstring, ob)));
}
_ sc_make_window(sc *sc) {
    return _sc_make_aref(sc, xwindow_new());
}
_ sc_window_config(sc *sc, _ win, _ disp) {
    xwindow_config(CAST(xwindow, win), CAST(xdisplay, disp));
    return VOID;
}


/*** GRID ***/

_ sc_make_grid_1(sc *sc, _ len, _ init) {
    return _sc_make_aref(sc, grid_new_1(CAST_INTEGER(len), CAST(inexact, init)->value));
}
_ sc_make_grid_2(sc *sc, _ l1, _ l2, _ init) {
    return _sc_make_aref(sc, grid_new_2(CAST_INTEGER(l1), CAST_INTEGER(l2), CAST(inexact, init)->value));
}
_ sc_grid_copy(sc *sc, _ ob) {
    return _sc_make_aref(sc, grid_copy(CAST(grid, ob)));
}

_ sc_grid_for_each(sc *sc, _ fn, _ gridv) {
    vector *v = CAST(vector, gridv);
    grid_proc *p = CAST(grid_proc, fn);
    int i, argc = vector_size(v);
    grid *argv[argc];
    for (i=0; i<argc; i++) argv[i] = CAST(grid, v->slot[i]);
    if (grid_for_each(p, argc, argv)) ERROR("args", CONS(fn, CONS(gridv, NIL)));
    return VOID;
}

static void _add(grid_atom *a, grid_atom *b, grid_atom *c) { *c = *a + *b; }
static void _sub(grid_atom *a, grid_atom *b, grid_atom *c) { *c = *a - *b; }
static void _mul(grid_atom *a, grid_atom *b, grid_atom *c) { *c = (*a) * (*b); }

_ grid_proc_wrap(sc *sc, void *fn, int argc) { 
    grid_proc *g = grid_proc_new(fn, argc);
    return _sc_make_aref(sc, g);
}

#define PROC(fn, nargs) return grid_proc_wrap(sc, fn, nargs);
_ sc_grid_proc_add(sc *sc) { PROC(_add, 3); }
_ sc_grid_proc_sub(sc *sc) { PROC(_sub, 3); }
_ sc_grid_proc_mul(sc *sc) { PROC(_mul, 3); }


_ sc_grid_dump(sc *sc, _ g, _ p) { 
    grid_dump(CAST(grid, g), CAST(port, p));  return VOID; 
}

_ sc_grid_to_vector(sc *sc, _ ob) {
    grid *g = CAST(grid, ob);
    int i, nb = grid_total(g);
    vector *v = gc_alloc(EX->gc, nb);
    for (i=0; i<nb; i++) {
        v->slot[i] = INEXACT(g->buf[i]);
    }
    return vector_to_object(v);
}
_ sc_grid_ref(sc *sc, _ obg, _ obi) {
    int i = CAST_INTEGER(obi);
    grid *g = CAST(grid, obg);
    int nb = grid_total(g);
    if ((i < 0) || (i >= nb)) ERROR("inval", obi);
    return INEXACT(g->buf[i]);
}
_ sc_bang_grid_set(sc *sc, _ obg, _ obi, _ obv) {
    int i = CAST_INTEGER(obi);
    grid *g = CAST(grid, obg);
    int nb = grid_total(g);
    if ((i < 0) || (i >= nb)) ERROR("inval", obi);
    g->buf[i] = CAST(inexact, obv)->value;
    return VOID;
}
_ sc_grid_dims(sc *sc, _ obg) {
    grid *g = CAST(grid, obg);
    vector *v = gc_alloc(EX->gc, g->rank);
    int i;
    for (i=0; i<grid_total(g); i++) {
        v->slot[i] = NUMBER(g->dim[i]);
    }
    return vector_to_object(v);
}
_ sc_grid_svd(sc *sc, _ A, _ V, _ S, _ work) {
    if (grid_linalg_SV_decomp(CAST(grid, A), CAST(grid, V), CAST(grid, S), CAST(grid, work))) {
        ERROR("arg", CONS(A, CONS(V, CONS(S, CONS(work, NIL)))));
    }
    return VOID;
}
_ sc_grid_svd_solve(sc *sc, _ U, _ V, _ S, _ b, _ x) {
    if (grid_linalg_SV_solve(CAST(grid, U), CAST(grid, V), CAST(grid, S),
                             CAST(grid, b), CAST(grid, x))) {
    }
    return VOID;
}

_ sc_vector_to_grid_1(sc *sc, _ vec) {
    vector *v = CAST(vector, vec);
    int i, len = vector_size(v);
    if (len < 1) return ERROR("invalid", vec);
    grid *g = grid_new_1(len, 0.0);
    for (i=0; i<len; i++) {
        inexact *x = object_to_inexact(v->slot[i], EX);
        g->buf[i] = x ? x->value : object_to_integer(v->slot[i]);
    }
    return _sc_make_aref(sc, g);
}

_ sc_vector_to_grid_2(sc *sc, _ vecofvec) {
    vector *v = CAST(vector, vecofvec);
    int rows = vector_size(v);
    if (rows < 1) return ERROR("invalid", rows);
    vector *vv = CAST(vector, v->slot[0]);
    int columns = vector_size(vv);
    grid *g = grid_new_2(columns, rows, 0.0);
    grid_atom *a;
    int i,j;
    for (j=0; j<rows; j++){
        a = &g->buf[j * columns];
        vv = object_to_vector(v->slot[j]);
        if (vv) {
            int c = vector_size(vv);
            if (c > columns) c = columns;
            for(i=0; i<c; i++) {
                inexact *x = object_to_inexact(vv->slot[i], EX);
                a[i] = x ? x->value : object_to_integer(vv->slot[i]);
            }
        }
    }
    return _sc_make_aref(sc, g);
}

/* Convert a grid into a hankel matrix. */
_ sc_grid_hankel(sc *sc, _ ob_sig, _ ob_cols) {
    int cols = CAST_INTEGER(ob_cols);
    grid *sig = CAST(grid, ob_sig);
    int total = grid_total(sig); // FIXME: use everything
    if ((cols < 1) || (cols > total)) ERROR("arg", ob_cols);
    int i,j;
    int rows = total+1-cols;
    grid *hank = grid_new_2(cols, rows, 0.0);
    grid_atom *s, *h = hank->buf;
    _ rv = _sc_make_aref(sc, hank);
    for(i=0; i<rows; i++){
        s = sig->buf + i;
        for(j=0; j<cols; j++) {
            *h++ = *s++;
        }
    }
    return rv;
}


_ sc_grid_mul_mv(sc *sc, _ A, _ x, _ y) {
    if (grid_blas_dgemv(0, 1.0, CAST(grid, A), CAST(grid, x), 1.0, CAST(grid, y))) {
        ERROR("arg", CONS(A, CONS(x, CONS(y, NIL))));
    }
    return VOID;
}


/*  Matrix unfold (for generating system output). */



_ sc_grid_unfold(sc *sc, _ A, _ x, _ out) {
    if (grid_unfold(CAST(grid, A), CAST(grid, x), CAST(grid, out))) {
        ERROR("arg", CONS(A, CONS(x, CONS(out, NIL))));
    }
    return VOID;
}

