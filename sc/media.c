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
static frame_class *frame_c;

/* Instead of storing the class object in the ex struct (see
   DEF_AREF_TYPE and ex's base_types member), it's also possible to
   store it in a global variable.  Here we use a _c postfix naming
   convention. */

#define DEF_GLOBAL_AREF_TYPE(name) \
    static inline name *object_to_##name(object ob, ex *m) { \
        return (name*)object_aref_struct(ob,m,name##_c); }

DEF_GLOBAL_AREF_TYPE(codec)
DEF_GLOBAL_AREF_TYPE(codec_context)
DEF_GLOBAL_AREF_TYPE(frame)

static prim_def media_prims[] = media_table_init;

void _sc_media_init(sc *sc) {
    av_register_all();
    codec_c = codec_class_new();
    codec_context_c = codec_context_class_new();
    frame_c = frame_class_new();
    _sc_def_prims(sc, media_prims);
}

_ sc_make_codec(sc* sc, _ spec) {
    char *name = CAST(cstring, spec);
    codec *c = codec_new(codec_c, name);
    if (!c) return ERROR("codec-not-found", spec);
    return _sc_make_aref(sc, &(codec_c->free), codec_new(codec_c, name));
}

_ sc_make_codec_context(sc *sc) {
    return _sc_make_aref(sc, &(codec_context_c->free), codec_context_new(codec_context_c));
}

_ sc_codec_context_info(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    _ex_printf(EX, "dim:  %d x %d\n", c->context->width, c->context->height);
    if (c->context->time_base.num == 1) 
        _ex_printf(EX, "fps:  %d\n", c->context->time_base.den);
    else
        _ex_printf(EX, "fps:  %d/%d\n", c->context->time_base.den, c->context->time_base.num);
    _ex_printf(EX, "rate: %d kbps\n", c->context->bit_rate / 1000);
    return VOID;
}

_ sc_make_frame(sc *sc, _ ob) {
    codec_context *c = CAST(codec_context, ob);
    frame *f = frame_new(frame_c, c);
    return _sc_make_aref(sc, &(frame_c->free), f);
}

_ sc_bang_frame_test(sc *sc, _ ob_frame, _ ob_ctx, _ ob_int) {
    int i = CAST_INTEGER(ob_int);
    frame *f = CAST(frame, ob_frame);
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
    frame *f = (frm == FALSE) ? NULL : CAST(frame, frm); // delayed frames
    codec_context_encode_video(CAST(codec_context, ctx), f, CAST(bytes, buf));
    return VOID;
}
