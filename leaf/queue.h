#ifndef _QUEUE_H_
#define _QUEUE_H_

/* Flat queue for real-time one-directional thread to thread message
   passing.  Only supports 1 read and 1 writer thread. */


#include <leaf/leaf.h>

#define QUEUE_ERR_OK     0
#define QUEUE_ERR_FULL  -1  /* Write end can not accomodate message. */
#define QUEUE_ERR_EMPTY -2  /* No more messages at read end. */
#define QUEUE_ERR_SIZE  -3  /* Size of message is incorrect. */

typedef unsigned int queue_index;
typedef unsigned int queue_size;


struct queue {
    LEAF_OBJECT(base); // FIXME: implement leaf object api
    unsigned char *buf;
    queue_index buf_size;

    /* Message state */
    queue_index index_read;
    queue_index index_write;

    /* Read transaction state */
    queue_index index_consume;
};
typedef struct queue queue;

queue *queue_new(queue_size bytes);

/* WRITE END */

/* Messages can be streamed, i.e. the writer doesn't need to know the
   size, but needs to check if a write is successful.  Unsuccessful
   writes will just abort. */

int  queue_write_open(queue *q);
int  queue_write_chunk(queue *q, const void *buf, queue_size bytes);
void queue_write_close(queue *q);

/* Single-function transaction */
int queue_write(queue *q, void *buf, queue_size bytes);


/* READ END */

int  queue_read_size(queue *q);
int  queue_read_open(queue *q);
int  queue_read_chunk(queue *q, void *buf, queue_size bytes);
void queue_read_close(queue *q);

/* Single-function transaction */
int queue_read(queue *q, void *buf, queue_size bytes);


#endif
