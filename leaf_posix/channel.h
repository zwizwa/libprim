/* Synchronous channels for leaf object communication between system
   threads and a main VM sequencer thread. 

   The channel is asymmetric: 1 reader, multiper writers.
*/

#ifndef _LEAF_CHANNEL_H_
#define _LEAF_CHANNEL_H_

#include "config.h"
#include <pthread.h>
#include <stdio.h>
#include <leaf/leaf.h>
#include <leaf/port.h>


typedef struct { leaf_class super; } channel_class;

typedef struct {
    leaf_object base;
    leaf_object *object;

    pthread_mutex_t mut;
    pthread_cond_t get_ok;
    pthread_cond_t put_ok;

    int rc;
    int teardown;
    pthread_cond_t teardown_ok;
    
} channel;

leaf_object *channel_get(channel *x);
int channel_put(channel *x, leaf_object *object);
int channel_get_would_block(channel *x);
int channel_put_would_block(channel *x);

leaf_class *channel_type(void);
channel* channel_new(void);


/* The main usage for channel objects is to connect to worker threads
   that perform I/O to prevent the main thread to block.  These
   functions add a reader or writer to a channel created with
   channel_new().  A port_writer will READ data from the channel and
   WRITE it to the port.  A port_reader does the reverse.  */

typedef leaf_object* (*channel_producer)(leaf_object *ctx);
typedef int (*channel_consumer)(leaf_object *ctx, leaf_object *msg);
 

// FIXME: make this generic (port = kind of ctx)
int channel_connect_consumer(channel *c, channel_consumer consume, leaf_object *ctx);
int channel_connect_producer(channel *c, channel_producer produce, leaf_object *ctx);

#endif
