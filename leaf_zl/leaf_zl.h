#ifndef _LEAF_ZL_
#define _LEAF_ZL_

/* Idea here is to wrap calls such that Swig can generate a wrapper. */

#include <zl/v4l.h>
#include <leaf/bytes.h>

void zl_v4l_next_bytes(struct zl_v4l *x, bytes *b, int block) {
    unsigned char *data;
    zl_v4l_next(x, &data, block);
    memcpy(b->bytes, data, b->size); // FIXME: check dims
}
unsigned int zl_v4l_fourcc(struct zl_v4l *x) {
    unsigned int fourcc, width, height;
    zl_v4l_get_format(x, &fourcc, &width, &height);
    return fourcc;
}
unsigned int zl_v4l_width(struct zl_v4l *x) {
    unsigned int fourcc, width, height;
    zl_v4l_get_format(x, &fourcc, &width, &height);
    return width;
}
unsigned int zl_v4l_height(struct zl_v4l *x) {
    unsigned int fourcc, width, height;
    zl_v4l_get_format(x, &fourcc, &width, &height);
    return height;
}
#endif
