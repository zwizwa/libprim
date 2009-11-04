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
typedef struct _frame frame;

typedef void (*free_m)(void *p);
typedef void (*codec_context_free_m)(codec_context *c);
typedef void (*frame_free_m)(frame *c);

typedef struct { free_m free; } codec_class;
typedef struct { codec_context_free_m free; } codec_context_class;
typedef struct { frame_free_m free; } frame_class;

struct _codec {
    codec_class *type;
    AVCodec *codec;
};
struct _codec_context {
    codec_context_class *type;
    AVCodecContext *context;
};
struct _frame {
    frame_class *type;
    AVFrame *frame;
    void *buf;
};



codec_class *codec_class_new(void);
codec_context_class *codec_context_class_new(void);
frame_class *frame_class_new(void);

codec *codec_new(codec_class *type, const char *name);
codec_context *codec_context_new(codec_context_class *type);
frame *frame_new(frame_class *type, codec_context *ctx);

void frame_test(frame *fram, codec_context *ctx, int i);

int codec_context_open(codec_context *ctx, codec *codec);
int codec_context_close(codec_context *ctx);

void codec_context_encode_video(codec_context *ctx, 
                                frame *f, 
                                bytes *b);

#endif
