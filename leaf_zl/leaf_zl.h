#ifndef _LEAF_ZL_
#define _LEAF_ZL_

#include <zl/v4l.h>
#include <leaf/bytes.h>

void zl_v4l_next_bytes(struct zl_v4l *x, bytes *b, int block) {
  unsigned char *data;
    zl_v4l_next(x, &data, block);
    memcpy(b->bytes, data, b->size); // FIXME: check dims
}

#endif
