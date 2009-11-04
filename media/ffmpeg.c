#include "ffmpeg.h"
#include "../leaf/bytes.h"

// from libavcodec/api-example.c
// http://www.irisa.fr/texmex/people/dufouil/ffmpegdoxy/apiexample_8c.html


// see "ffmpeg -formats"

/** CLASSES **/

/* CODEC */
codec *codec_new(codec_class *type, const char *name) {
    AVCodec *c = avcodec_find_encoder_by_name(name);
    if (!c) return NULL;
    codec *x = malloc(sizeof(*x));
    x->type = type;
    x->codec = c;
    return x;
}
codec_class *codec_class_new(void) {
    codec_class *x = malloc(sizeof(*x)); 
    x->free = free; 
    return x; 
}


/* CODEC CONTEXT */
static void codec_context_free(codec_context *c) {
    av_freep(&c->context);
    free(c);
};
codec_context *codec_context_new(codec_context_class *type){
    codec_context *x = malloc(sizeof(*x));
    x->type = type;
    x->context = avcodec_alloc_context();

    /* FIXME: parameterize */
    x->context->bit_rate = 400000;
    x->context->width = 352;
    x->context->height = 288;
    x->context->time_base= (AVRational){1,25};
    x->context->gop_size = 10; /* emit one intra frame every ten frames */
    x->context->max_b_frames=1;
    x->context->pix_fmt = PIX_FMT_YUV420P;

    return x;
}
codec_context_class *codec_context_class_new(void) {
    codec_context_class *x = malloc(sizeof(*x)); 
    x->free = codec_context_free;
    return x; 
}


/* FRAME */
frame *frame_new(frame_class *type, codec_context *ctx) {
    if (ctx->context->pix_fmt != PIX_FMT_YUV420P) return NULL;
    int size = ctx->context->width * ctx->context->height;

    // printf("size %d\n", size);

    int buf_size = (size * 3) / 2;
    frame *x = malloc(sizeof(*x));
    x->type = type;
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
static void frame_free(frame *f) {
    av_free(f->frame);
    free(f->buf);
    free(f);
}
frame_class *frame_class_new(void) {
    frame_class *x = malloc(sizeof(*x)); 
    x->free = frame_free;
    return x; 
}

void frame_test(frame *fram, codec_context *ctx, int i) {

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

// void codec_open(codec_context *ctx, 

int codec_context_open(codec_context *ctx, codec *cod) {
    return avcodec_open(ctx->context, cod->codec);
}
int codec_context_close(codec_context *ctx) {
    if (!ctx->context->codec) return -1;
    return avcodec_close(ctx->context);
}

void codec_context_encode_video(codec_context *ctx, 
                                frame *f, 
                                bytes *b) {
    b->size = avcodec_encode_video(ctx->context, 
                                   (uint8_t *)b->bytes, 
                                   b->bufsize, 
                                   f->frame);
}
