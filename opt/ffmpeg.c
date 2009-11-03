#include "ffmpeg.h"

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




void video_encode_example(const char *filename)
{
    codec *cod;
    codec_context *ctx= NULL;
    int i, out_size, size, x, y, outbuf_size;
    FILE *f;
    AVFrame *picture;
    uint8_t *outbuf, *picture_buf;

    av_register_all();

    cod = codec_new(&codec_c, "mpeg1video");
    ctx = codec_context_new(&codec_context_c);

    picture= avcodec_alloc_frame();


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

    /* alloc image and output buffer */
    outbuf_size = 100000;
    outbuf = malloc(outbuf_size);
    size = ctx->context->width * ctx->context->height;
    picture_buf = malloc((size * 3) / 2); /* size for YUV 420 */

    picture->data[0] = picture_buf;
    picture->data[1] = picture->data[0] + size;
    picture->data[2] = picture->data[1] + size / 4;
    picture->linesize[0] = ctx->context->width;
    picture->linesize[1] = ctx->context->width / 2;
    picture->linesize[2] = ctx->context->width / 2;

    /* encode 1 second of video */
    for(i=0;i<25;i++) {
        fflush(stdout);
        /* prepare a dummy image */
        /* Y */
        for(y=0;y<ctx->context->height;y++) {
            for(x=0;x<ctx->context->width;x++) {
                picture->data[0][y * picture->linesize[0] + x] = x + y + i * 3;
            }
        }

        /* Cb and Cr */
        for(y=0;y<ctx->context->height/2;y++) {
            for(x=0;x<ctx->context->width/2;x++) {
                picture->data[1][y * picture->linesize[1] + x] = 128 + y + i * 2;
                picture->data[2][y * picture->linesize[2] + x] = 64 + x + i * 5;
            }
        }

        /* encode the image */
        out_size = avcodec_encode_video(ctx->context, outbuf, outbuf_size, picture);
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
    free(picture_buf);
    free(outbuf);

    ctx->type->free(ctx);
    cod->type->free(cod);

    av_free(picture);
    printf("\n");
}
