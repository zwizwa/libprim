
#include "leaf/queue.h"
#include <string.h>

#define ASSERT LEAF_ASSERT
#define LOG    LEAF_LOG

void buf_fill(unsigned char *buf, int size) {
    int i;
    for (i=0; i<size; i++) buf[i] = i;
}
bool buf_check(unsigned char *buf, int size) {
    int i;
    for (i=0; i<size; i++) {
        if (buf[i] != i) {
            LOG("buf_check: %d %d", i, buf[i]);
            return false;
        }
    }
    return true;
}

int main(void) {
    LOG("queue test");
    queue *x = queue_new(128);
    ASSERT(x!=NULL);

    unsigned char v[23] = {};

    int i;
    for (i=0; i<100; i++) {
        LOG("loop %d", i);

        buf_fill(v, sizeof(v));
        ASSERT(QUEUE_ERR_OK == queue_write_open(x));
        ASSERT(QUEUE_ERR_OK == queue_write_append(x,&v,sizeof(v)));
        ASSERT(QUEUE_ERR_OK == queue_write_append(x,&v,sizeof(v)));
        queue_write_close(x);

        ASSERT(QUEUE_ERR_OK == queue_write_open(x));
        ASSERT(QUEUE_ERR_OK == queue_write_append(x,&v,sizeof(v)));
        queue_write_close(x);

        ASSERT(QUEUE_ERR_OK == queue_read_open(x));
        bzero(&v, sizeof(v)); ASSERT(QUEUE_ERR_OK == queue_read_consume(x, &v, sizeof(v))); ASSERT(buf_check(v, sizeof(v)));
        bzero(&v, sizeof(v)); ASSERT(QUEUE_ERR_OK == queue_read_consume(x, &v, sizeof(v))); ASSERT(buf_check(v, sizeof(v)));
        queue_read_close(x);

        ASSERT(QUEUE_ERR_OK == queue_read_open(x));
        bzero(&v, sizeof(v)); ASSERT(QUEUE_ERR_OK == queue_read_consume(x, &v, sizeof(v))); ASSERT(buf_check(v, sizeof(v)));
        queue_read_close(x);
    }

    // ASSERT(0);
    return 0;
}
