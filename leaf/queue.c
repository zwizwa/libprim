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

/* Leave 1 bytes room to remove empty/full ambiguity. */
#define QUEUE_SPACER 1
static queue_size queue_write_room(struct queue *x) {
    return queue_index_distance(x, x->index_write + QUEUE_SPACER, x->index_read);
}

static queue_index queue_linear_room(struct queue *x, queue_index i) {
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
    if (sizeof(queue_size) > queue_write_room(x)) return QUEUE_ERR_FULL;
    write_queue_size(x, x->index_write, 0);
    return QUEUE_ERR_OK;
}


int queue_write_chunk(queue *x, const void *_buf, queue_size bytes) {
    if (bytes > queue_write_room(x)) return QUEUE_ERR_FULL;

    /* In-queue message is prefixed with message size. */
    queue_size msg_bytes = read_queue_size(x, x->index_write);

    /* Transfer 1 or 2 chunks. */
    const unsigned char *buf = _buf;
    queue_index room, chunk;
    while (bytes > 0) {

        /* Where to write? */
        queue_index index_append = x->index_write;
        queue_index_increment(x, &index_append, sizeof(queue_size) + msg_bytes);

        /* Copy chunk */
        room = queue_linear_room(x, index_append);
        chunk = (bytes < room) ? bytes : room;
        memcpy(x->buf + index_append, buf, chunk);

        /* Advance to next chunk */
        buf += chunk;
        bytes -= chunk;
        msg_bytes += chunk;
    }
    write_queue_size(x, x->index_write, msg_bytes);
    return QUEUE_ERR_OK;
}
void queue_write_close(queue *x) {
    queue_size msg_bytes = read_queue_size(x, x->index_write);
    queue_index_increment(x, &x->index_write, sizeof(queue_size) + msg_bytes);
}



int queue_read_open(queue *x) {
    if (x->index_read == x->index_write) return QUEUE_ERR_EMPTY;
    queue_index index_consume = x->index_read;
    queue_index_increment(x, &index_consume, sizeof(queue_size));
    return QUEUE_ERR_OK;
}

int queue_read_chunk(queue *x, void *_buf, queue_size bytes) {
    unsigned char *buf = _buf;
    queue_index room, chunk;
    while (bytes > 0) {
        /* Copy chunk */
        room = queue_linear_room(x, x->index_consume);
        chunk = (bytes < room) ? bytes : room;
        memcpy(buf, x->buf + x->index_consume, chunk);
        /* Advance to next chunk */
        buf += chunk;
        bytes -= chunk;
        queue_index_increment(x, &x->index_consume, chunk);
    }
    return QUEUE_ERR_OK;
}
