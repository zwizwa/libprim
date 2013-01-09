#include "leaf/queue.h"

#define ASSERT LEAF_ASSERT
#define LOG    LEAF_LOG

int main(void) {
    LOG("queue test");
    queue *x = queue_new(128);
    ASSERT(x!=NULL);
    ASSERT(QUEUE_ERR_OK == queue_write_open(x));
    int v = 123;
    ASSERT(QUEUE_ERR_OK == queue_write_chunk(x,&v,sizeof(v)));
    queue_write_close(x);
    // ASSERT(0);
    return 0;
}
