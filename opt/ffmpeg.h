#ifndef __FFMPEG_H__
#define __FFMPEG_H__

#include <libavcodec/avcodec.h>

/* 
    Goal: bridge internal image representation with FFmpeg encoder.
*/


typedef struct {
} codec_class;

typedef struct _codec codec;
struct _codec {
    codec_class *type;
    AVCodec *codec;
};


#endif
