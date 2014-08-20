#if 1 // not currently working

#include "ffmpeg.h"
#include "../leaf/bytes.h"
#include <stdio.h>
// from libavcodec/api-example.c
// http://www.irisa.fr/texmex/people/dufouil/ffmpegdoxy/apiexample_8c.html

#include <leaf/port.h>

// see "ffmpeg -formats"

/** CLASSES **/

/* CODEC */
static int codec_write(codec *c, port *p) {
    return port_printf(p, "#<codec:%s>", c->codec->name);
}
codec_class *codec_type(void) {
    static codec_class *x = NULL;
    if (!x) {
        codec_class *x = calloc(1, sizeof(*x)); 
        leaf_class_init((leaf_class*)x, (leaf_free_m)free, (leaf_write_m)codec_write);
    }
    return x; 
}

codec *codec_new(const char *name) {
    AVCodec *c = avcodec_find_encoder_by_name(name);
    if (!c) return NULL;
    codec *x = malloc(sizeof(*x));
    x->type = codec_type();
    x->codec = c;
    return x;
}


/* CODEC CONTEXT */
static void codec_context_free(codec_context *c) {
    av_freep(&c->context);
    free(c);
};
static int codec_context_write(codec_context *c, port *p) {
    return port_printf(p, "#<codec_context:%p>", c);
}
codec_context_class *codec_context_type(void) {
    static codec_context_class *x = NULL;
    if (!x) {
        x = calloc(1, sizeof(*x));
        leaf_class_init((leaf_class*)x,
                        (leaf_free_m)codec_context_free,
                        (leaf_write_m)codec_context_write);
    }
    return x; 
}
codec_context *codec_context_new(void){
    codec_context *x = malloc(sizeof(*x));
    x->type = codec_context_type();
    x->context = avcodec_alloc_context3(NULL);

    /* FIXME: parameterize */

    /* video */
    if (1) {
        x->context->bit_rate = 400000;
        x->context->width = 352;
        x->context->height = 288;
        x->context->time_base= (AVRational){1,25};
        x->context->gop_size = 10; /* emit one intra frame every ten frames */
        x->context->max_b_frames=1;
        x->context->pix_fmt = PIX_FMT_YUV420P;
    }

    /* audio */
    if (1) {
        x->context->sample_rate = 44100;
        x->context->channels = 2;
    }

    return x;
}




/* FRAME */
static int vframe_write(vframe *x, port *p) {
    return port_printf(p, "#<vframe:%p>", x);
}
static void vframe_free(vframe *f) {
    av_free(f->frame);
    free(f->buf);
    free(f);
}
vframe_class *vframe_type(void) {
    static vframe_class *x = NULL;
    if (!x) {
        x = calloc(1, sizeof(*x)); 
        leaf_class_init((leaf_class*)x,
                        (leaf_free_m)vframe_free,
                        (leaf_write_m)vframe_write);
    }
    return x; 
}
vframe *vframe_new(codec_context *ctx) {
    if (ctx->context->pix_fmt != PIX_FMT_YUV420P) return NULL;
    int size = ctx->context->width * ctx->context->height;

    // printf("size %d\n", size);

    int buf_size = (size * 3) / 2;
    vframe *x = malloc(sizeof(*x));
    x->type = vframe_type();
    x->buf = malloc(buf_size);
    x->frame = avcodec_alloc_frame();

    x->frame->data[0] = x->buf;
    x->frame->data[1] = x->frame->data[0] + size;
    x->frame->data[2] = x->frame->data[1] + size / 4;

    x->frame->linesize[0] = ctx->context->width;
    x->frame->linesize[1] = ctx->context->width / 2;
    x->frame->linesize[2] = ctx->context->width / 2;

    return x;
}

static void aframe_write(aframe *x, port *p) {
    port_printf(p, "#<aframe:%p>", x);
}
static void aframe_free(aframe *f) {
    free(f->samples);
    free(f);
}
aframe_class *aframe_type(void) {
    static aframe_class *x = NULL;
    if (!x) {
        x = calloc(1, sizeof(*x)); 
        leaf_class_init((leaf_class*)x,
                        (leaf_free_m)aframe_free,
                        (leaf_write_m)aframe_write);
    }
    return x; 
}


aframe *aframe_new(codec_context *ctx) {
    aframe *x = malloc(sizeof(*x));
    int size = 2 * ctx->context->frame_size * ctx->context->channels;
    if (!size) return NULL;
    x->type = aframe_type();
    x->samples = malloc(size);
    return x;
}


void frame_test(vframe *fram, codec_context *ctx, int i) {

    int x,y;
    /* prepare a dummy image */
    /* Y */
    for(y=0;y<ctx->context->height;y++) {
        for(x=0;x<ctx->context->width;x++) {
            fram->frame->data[0][y * fram->frame->linesize[0] + x] = x + y + i * 3;
        }
    }
    
    /* Cb and Cr */
    for(y=0;y<ctx->context->height/2;y++) {
        for(x=0;x<ctx->context->width/2;x++) {
            fram->frame->data[1][y * fram->frame->linesize[1] + x] = 128 + y + i * 2;
            fram->frame->data[2][y * fram->frame->linesize[2] + x] = 64 + x + i * 5;
        }
    }
}


/* Aframe */



// void codec_open(codec_context *ctx, 

int codec_context_open(codec_context *ctx, codec *cod) {
    return avcodec_open(ctx->context, cod->codec);
}
int codec_context_close(codec_context *ctx) {
    if (!ctx->context->codec) return -1;
    return avcodec_close(ctx->context);
}

void codec_context_encode_video(codec_context *ctx, 
                                vframe *f, 
                                bytes *b) {
    b->size = avcodec_encode_video(ctx->context, 
                                   (uint8_t *)b->bytes, 
                                   b->bufsize, 
                                   f ? f->frame : NULL);
}

void codec_context_encode_audio(codec_context *ctx, 
                                aframe *f, 
                                bytes *b) {
    b->size = avcodec_encode_audio(ctx->context, 
                                   (uint8_t *)b->bytes, 
                                   b->bufsize, 
                                   f ? f->samples : NULL);
}

void codec_context_info(codec_context *c, port *p) {
    port_printf(p, "video:\n");
    port_printf(p, " dim:  %d x %d\n", c->context->width, c->context->height);
    if (c->context->time_base.num == 1) 
        port_printf(p, " fps:  %d\n", c->context->time_base.den);
    else
        port_printf(p, " fps:  %d/%d\n", 
                c->context->time_base.den, 
                c->context->time_base.num);
    port_printf(p, " rate: %d kbps\n", c->context->bit_rate / 1000);
    port_printf(p, "audio:\n");
    port_printf(p, " sr:   %d Hz\n", c->context->sample_rate);
    port_printf(p, " chan: %d\n", c->context->channels);
    port_printf(p, " blk:  %d\n", c->context->frame_size);    
}


#endif
