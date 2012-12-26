#ifndef _MEDIA_YUV_H_
#define _MEDIA_YUV_H_

#include <leaf/leaf.h>

typedef struct { LEAF_CLASS(super); } yuv_class;


#define FOURCC_YV12 0x32315659 ///< standard YV12
#define FOURCC_I420 0x30323449 ///< standard I420

#define FOURCC_UYVY 0x59565955 ///< standard UYVY
#define FOURCC_YUY2 0x32595559 ///< standard YUY2

typedef struct {
    yuv_class *type;
    char *buf;
    int bufsize;
    int width;
    int height;
    int fourcc;
} yuv;

yuv_class *yuv_type(void);
yuv *yuv_new(int w, int h, const char *fourcc);

#endif
