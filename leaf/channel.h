/* Synchronous channels for leaf object communication between system
   threads and a main VM sequencer thread. 

   The channel is asymmetric: 1 reader, multiper writers.
*/

#ifndef _LEAF_CHANNEL_H_
#define _LEAF_CHANNEL_H_

#include <pthread.h>
#include <stdio.h>
#include <leaf/leaf.h>
#include <leaf/port.h>


typedef struct { leaf_class super; } channel_class;

typedef struct {
    channel_class *type;
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

channel_class *channel_type(void);
channel* channel_new(void);


/* The main usage for channel objects is to connect to worker threads
   that perform I/O to prevent the main thread to block.  These
   functions add a reader or writer to a channel created with
   channel_new().  A port_writer will READ data from the channel and
   WRITE it to the port.  A port_reader does the reverse.  */

typedef leaf_object* (*port_reader)(leaf_object *ctx, port *p);
typedef int (*port_writer)(leaf_object *ctx, port *p, leaf_object *ob);
 
int channel_connect_port_writer(channel *c, port *p, port_writer write, void *ctx);
int channel_connect_port_reader(channel *c, port *p, port_reader read, void *ctx);

#endif
