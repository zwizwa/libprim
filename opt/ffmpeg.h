#ifndef __FFMPEG_H__
#define __FFMPEG_H__

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

/*  Bridge internal image representation with FFmpeg encoder.

    FFmpeg is represented by its standard objects: frame, codec and
    context (configuration data for codec).

*/

typedef struct _codec codec;
typedef struct _codec_context codec_context;
typedef struct _frame frame;

typedef void (*free_m)(void *p);
typedef void (*codec_context_free_m)(codec_context *c);


typedef struct {
    free_m free;
} codec_class;

typedef struct {
    codec_context_free_m free;
} codec_context_class;

typedef struct {
    free_m free;
} frame_class;


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
};


codec* codec_new(codec_class *type, const char *name);


#endif
