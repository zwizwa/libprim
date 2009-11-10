/* libprim/media bindings for Scheme

   This file bridges the Scheme memory / control (exception) model
   with low-level C objects.  The function prototypes are mined to
   produce media.h_sc_prims contating media_table_init + macro defs.

 */

#include "scheme.h"
#include <media/ffmpeg.h>
#include <leaf/bytes.h>
#include "media.h_sc_prims"

static codec_class *codec_c;
static codec_context_class *codec_context_c;
static vframe_class *vframe_c;
static aframe_class *aframe_c;

/* Instead of storing the class object in the ex struct (see
   DEF_AREF_TYPE and ex's base_types member), it's also possible to
   store it in a global variable.  Here we use a _c postfix naming
   convention. */

#define DEF_GLOBAL_AREF_TYPE(name) \
    static inline name *object_to_##name(object ob, ex *m) { \
        return (name*)object_aref_struct(ob,m,name##_c); }

DEF_GLOBAL_AREF_TYPE(codec)
DEF_GLOBAL_AREF_TYPE(codec_context)
DEF_GLOBAL_AREF_TYPE(vframe)
DEF_GLOBAL_AREF_TYPE(aframe)

static prim_def media_prims[] = media_table_init;

void _sc_media_init(sc *sc) {
    av_register_all();
    codec_c = codec_class_new();
    codec_context_c = codec_context_class_new();
    vframe_c = vframe_class_new();
    aframe_c = aframe_class_new();
    _sc_def_prims(sc, media_prims);
}

/* Wrappers: these should be generated, as they simply bridge the LEAF
   objects to their wrappers for SC + bridge stdout and errors. */

_ sc_make_codec(sc* sc, _ spec) {
    char *name = CAST(cstring, spec);
    codec *c = codec_new(codec_c, name);
    if (!c) return ERROR("codec-not-found", spec);
    return _sc_make_aref(sc, (leaf_object*)codec_new(codec_c, name));
}

_ sc_make_codec_context(sc *sc) {
    return _sc_make_aref(sc, (leaf_object *)codec_context_new(codec_context_c));
}


_ sc_codec_context_info(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    codec_context_info(c, _sc_port(sc));
    return VOID;
}

_ sc_make_vframe(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    vframe *f = vframe_new(vframe_c, c);
    return _sc_make_aref(sc, (leaf_object *)f);
}

_ sc_make_aframe(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    aframe *f = aframe_new(aframe_c, c);
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
