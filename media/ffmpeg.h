#ifndef __FFMPEG_H__
#define __FFMPEG_H__

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

#include <leaf/bytes.h>

/*  Bridge internal image representation with FFmpeg encoder.

    FFmpeg is represented by its standard objects: frame, codec and
    context (configuration data for codec).

*/

typedef struct _codec codec;
typedef struct _codec_context codec_context;
typedef struct _vframe vframe;
typedef struct _aframe aframe;

typedef struct { LEAF_CLASS(super); } codec_class;
typedef struct { LEAF_CLASS(super); } codec_context_class;
typedef struct { LEAF_CLASS(super); } vframe_class;
typedef struct { LEAF_CLASS(super); } aframe_class;

struct _codec {
    codec_class *type;
    AVCodec *codec;
};
struct _codec_context {
    codec_context_class *type;
    AVCodecContext *context;
};
struct _vframe {
    vframe_class *type;
    AVFrame *frame;
    void *buf;
};
struct _aframe {
    aframe_class *type;
    short int *samples;
};



codec_class *codec_type(void);
codec_context_class *codec_context_type(void);
vframe_class *vframe_type(void);
aframe_class *aframe_type(void);

codec *codec_new(const char *name);
codec_context *codec_context_new(void);
vframe *vframe_new(codec_context *ctx);
aframe *aframe_new(codec_context *ctx);

void frame_test(vframe *fram, codec_context *ctx, int i);

int codec_context_open(codec_context *ctx, codec *codec);
int codec_context_close(codec_context *ctx);

void codec_context_encode_video(codec_context *ctx, vframe *f, bytes *b);
void codec_context_encode_audio(codec_context *ctx, aframe *f, bytes *b);

void codec_context_info(codec_context *c, port *p);

#endif
