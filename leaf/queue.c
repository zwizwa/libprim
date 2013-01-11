#include "queue.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#define LOG    LEAF_LOG

#if 1
#define ASSERT LEAF_ASSERT
#else
#define ASSERT(x)
#endif


queue *queue_new(queue_size bytes) {
    queue *x = calloc(1,sizeof(*x));
    x->buf = calloc(1,bytes);
    x->buf_size = bytes;
    return x;
}

static queue_index queue_index_distance(struct queue *x, queue_index begin, queue_index endx) {
    queue_index room = (x->buf_size + endx - begin) % x->buf_size;
    return room;
}


static queue_index queue_until_buf_end(struct queue *x, queue_index i) {
    ASSERT(i < x->buf_size);
    return x->buf_size - i;
}
static void queue_index_increment(struct queue *x,
                                  queue_index *i, queue_index inc) {
    ASSERT((*i) < x->buf_size);
    queue_index i_inc = *i + inc;
    while (i_inc >= x->buf_size) i_inc -= x->buf_size;
    *i = i_inc;
}

/* WRITE */


/* Leave 1 bytes room to remove empty/full ambiguity. */
#define QUEUE_SPACER 1
static queue_size queue_write_room(struct queue *x, queue_size message_bytes) {
    return queue_index_distance(x, x->index_write
        + sizeof(queue_index) + message_bytes + QUEUE_SPACER, x->index_read);
}

/* Read/write indices into queue */
static inline void write_queue_size(struct queue *x, queue_index i, queue_size s) {
    ASSERT(i < x->buf_size);
    memcpy(x->buf + i, &s, sizeof(s));
}
static inline queue_size read_queue_size(struct queue *x, queue_index i) {
    ASSERT(i < x->buf_size);
    queue_size s;
    memcpy(&s, x->buf + i, sizeof(s));
    return s;
}


int queue_write_open(queue *x) {
    /* Message size is stored in the first word in the queue. */
    if (queue_write_room(x, 0) == 0) return QUEUE_ERR_FULL;
    x->size_build = 0;
    return QUEUE_ERR_OK;
}



int queue_write_append(queue *x, const void *_buf, queue_size bytes) {

    queue_size r = queue_write_room(x, x->size_build);
    if (bytes > r) return QUEUE_ERR_FULL;

    /* Transfer 1 or 2 chunks. */
    const unsigned char *buf = _buf;
    while (bytes > 0) {

        /* Where to write? */
        queue_index index_append = x->index_write;
        queue_index_increment(x, &index_append, sizeof(queue_size) + x->size_build);

        /* Copy chunk */
        queue_size chunk = queue_until_buf_end(x, index_append);
        if (bytes < chunk) chunk = bytes;
        memcpy(x->buf + index_append, buf, chunk);

        /* Advance to next chunk */
        buf += chunk;
        bytes -= chunk;
        x->size_build += chunk;
    }
    return QUEUE_ERR_OK;
}
void queue_write_close(queue *x) {
    write_queue_size(x, x->index_write, x->size_build);
    queue_index_increment(x, &x->index_write, sizeof(queue_size) + x->size_build);
    x->size_build = 0;
}

#define TRY(x) if (QUEUE_ERR_OK != (err = x)) return err
int queue_write(queue *x, const void *buf, queue_size bytes) {
    int err;
    TRY(queue_write_open(x));
    TRY(queue_write_append(x, buf, bytes));
    queue_write_close(x);
    return QUEUE_ERR_OK;
}

/* READ */


int queue_read_open(queue *x) {
    if (x->index_read == x->index_write) return QUEUE_ERR_EMPTY;
    x->size_consume = 0;
    return QUEUE_ERR_OK;
}

int queue_read_consume(queue *x, void *_buf, queue_size bytes) {
    queue_size msg_size = read_queue_size(x, x->index_read);
    if (bytes + x->size_consume > msg_size) return QUEUE_ERR_SIZE;

    unsigned char *buf = _buf;
    while (bytes > 0) {

        /* Where to read? */
        queue_index index_consume = x->index_read;
        queue_index_increment(x, &index_consume, sizeof(queue_size) + x->size_consume);

        /* Copy chunk */
        queue_size chunk = queue_until_buf_end(x, index_consume);
        if (bytes < chunk) chunk = bytes;
        memcpy(buf, x->buf + index_consume, chunk);

        /* Advance to next chunk */
        buf += chunk;
        bytes -= chunk;
        x->size_consume += chunk;
    }
    return QUEUE_ERR_OK;
}
void queue_read_close(queue *x) {
    if (x->index_read == x->index_write) return;
    queue_size msg_size = read_queue_size(x, x->index_read);
    if (x->size_consume != msg_size) {
        LOG("WARNING: queue_read_close() not all bytes read %d/%d",
            x->size_consume, msg_size);
    }
    queue_index_increment(x, &x->index_read, sizeof(queue_size) + msg_size);
    x->size_consume = 0;
}

bool queue_read_ready(queue *x) {
    return (x->index_read != x->index_write);
}
