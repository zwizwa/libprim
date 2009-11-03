#include "ffmpeg.h"
#include "../leaf/bytes.h"

// from libavcodec/api-example.c
// http://www.irisa.fr/texmex/people/dufouil/ffmpegdoxy/apiexample_8c.html


// see "ffmpeg -formats"

/** CLASSES **/

/* CODEC */
codec *codec_new(codec_class *type, const char *name) {
    AVCodec *c = avcodec_find_encoder_by_name(name);
    if (!c) { 
        fprintf(stderr, "codec `%s' not found\n", name);
        exit(1); // primitives need exceptions!
        return NULL; 
    }
    codec *x = malloc(sizeof(*x));
    x->type = type;
    x->codec = c;
    return x;
}
codec_class codec_c = {free};  // no short-lived delegates


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
codec_context_class codec_context_c = {codec_context_free};


/* FRAME */
frame *frame_new(frame_class *type, codec_context *ctx) {
    if (ctx->context->pix_fmt != PIX_FMT_YUV420P) {
        fprintf(stderr, "expected PIX_FMT_YUV420P\n");
        exit(1); // primitives need exceptions!
        return NULL; 
    }
    int size = ctx->context->width * ctx->context->height;

    printf("size %d\n", size);

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
frame_class frame_c = {frame_free};

void test_frame(frame *fram, codec_context *ctx, int i) {

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


void encode_video(codec_context *ctx, 
                  frame *f, 
                  bytes *b) {
    b->size = avcodec_encode_video(ctx->context, 
                                   (uint8_t *)b->bytes, 
                                   b->bufsize, 
                                   f->frame);
}


void video_encode_example(const char *filename)
{
    codec *cod;
    codec_context *ctx= NULL;
    int out_size;
    int i;
    FILE *f;
    frame *fram;

    av_register_all();

    cod = codec_new(&codec_c, "mpeg1video");
    ctx = codec_context_new(&codec_context_c);

    fram = frame_new(&frame_c, ctx);

    int outbuf_size = 100000;
    uint8_t *outbuf = (uint8_t*)malloc(outbuf_size); 

    /* open it */
    if (avcodec_open(ctx->context, cod->codec) < 0) {
        fprintf(stderr, "could not open codec\n");
        exit(1);
    }

    f = fopen(filename, "wb");
    if (!f) {
        fprintf(stderr, "could not open %s\n", filename);
        exit(1);
    }


    /* encode 1 second of video */
    for(i=0;i<25;i++) {
        fflush(stdout);

        test_frame(fram, ctx, i);

        /* encode the image */
        out_size = avcodec_encode_video(ctx->context, outbuf, outbuf_size, fram->frame);
        printf("encoding frame %3d (size=%5d)\n", i, out_size);
        fwrite(outbuf, 1, out_size, f);
    }

    /* get the delayed frames */
    for(; out_size; i++) {
        fflush(stdout);

        out_size = avcodec_encode_video(ctx->context, outbuf, outbuf_size, NULL);
        printf("write frame %3d (size=%5d)\n", i, out_size);
        fwrite(outbuf, 1, out_size, f);
    }

    /* add sequence end code to have a real mpeg file */
    outbuf[0] = 0x00;
    outbuf[1] = 0x00;
    outbuf[2] = 0x01;
    outbuf[3] = 0xb7;
    fwrite(outbuf, 1, 4, f);
    fclose(f);
    free(outbuf);

    /* cleanup */
    ctx->type->free(ctx);
    cod->type->free(cod);
    fram->type->free(fram);

    printf("\n");
}
