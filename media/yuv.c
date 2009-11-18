#include <media/yuv.h>
#include <leaf/port.h>
#include <stdlib.h>
#include <string.h>

static int yuv_write(yuv *x, port *p) {
    char fmt[5] = {0,0,0,0,0};
    memcpy(fmt, &x->fourcc, 4);
    return port_printf(p, "#<%s:%dx%d>", fmt, x->width, x->height);
}
static void yuv_free(yuv *x) {
    free(x->buf);
    free(x);
}
int yuv_dump(yuv *x, port *p) {
    return port_write(p, x->buf, x->bufsize);
}
yuv_class *yuv_type(void) {
    static yuv_class *x = NULL;
    if (!x) {
        x = calloc(1, sizeof(*x));
        x->super.free  = (leaf_free_m)yuv_free;
        x->super.write = (leaf_write_m)yuv_write;
        x->super.dump  = (leaf_write_m)yuv_dump;
    }
    return x;
}

yuv *yuv_new(int w, int h, const char *fourcc_str) {
    int i, bufsize, fourcc = 0;
    char *str = (char *)&fourcc;
    for (i=0; (i<4) && fourcc_str[i]; i++) str[i] = fourcc_str[i];

    if ((w < 2) || (h < 2)) return NULL;
    switch(fourcc) {
    case FOURCC_YV12:
    case FOURCC_I420:
        bufsize = w * h;
        bufsize += bufsize/2;
        break;
    case FOURCC_YUY2:
    case FOURCC_UYVY:
        bufsize = 2 * w * h;
        break;
    default:
        return NULL;
    }
    yuv *x = calloc(1, sizeof(*x));
    x->type = yuv_type();
    x->width = w;
    x->height = h;
    x->fourcc = fourcc;
    x->bufsize = bufsize;
    x->buf = malloc(bufsize);
    return x;
}
