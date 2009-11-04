/* libprim/media bindings for Scheme */

#include "scheme.h"
#include <media/ffmpeg.h>
#include <leaf/bytes.h>
#include "media.h_sc_prims"

codec_class *codec_c;
codec_context_class *codec_context_c;

static prim_def media_prims[] = media_table_init;

void _sc_media_init(sc *sc) {
    codec_c = codec_class_new();
    codec_context_c = codec_context_class_new();
    // def prims
}

_ sc_make_codec(sc* sc, _ spec) {
    char *name = object_to_cstring(spec, &sc->m);
    if (!name) return TYPE_ERROR(spec);
    return _sc_make_aref(sc, &(codec_c->free), name);
}
