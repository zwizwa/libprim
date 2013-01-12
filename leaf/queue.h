#ifndef _QUEUE_H_
#define _QUEUE_H_

/* Flat queue for real-time one-directional thread to thread message
   passing.  Only supports 1 read and 1 writer thread.  The main
   purpose of this is the communication between a high and low
   priority thread (i.e. audio synth core and gui).

   Why yet another queue implementation?
   The devil is in the details..

   - No locks: this is a 1->1 queue with synchronization based on
     atomic read/write of the index_read and index_write fields.

   - No need for multiple data passes: _write allows multiple appends
     without needing to know the size; the message just has to fit.
     Writes that are not closed will be discarded.

   Especially the combination of incremental streaming writes (no
   multiple passes, no need for large buffers to hold intermediate
   messages) and no-hassle abort is quite useful in practice: it keeps
   the use side very simple.  */


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

    /* Read/write transaction state */
    queue_size size_build;
    queue_size size_consume;
};
typedef struct queue queue;

queue *queue_new(queue_size bytes);

/* WRITE END */

/* Messages can be streamed, i.e. the writer doesn't need to know the
   size, but needs to check if a write is successful.  Unsuccessful
   writes will just abort. */

int  queue_write_open(queue *q);
int  queue_write_append(queue *q, const void *buf, queue_size bytes);
void queue_write_close(queue *q);

/* READ END */

int  queue_read_size(queue *q);
int  queue_read_open(queue *q);
int  queue_read_consume(queue *q, void *buf, queue_size bytes);
void queue_read_close(queue *q);
bool queue_read_ready(queue *q);

#endif
