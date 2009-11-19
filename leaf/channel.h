/* Synchronous channels for leaf object communication between system
   threads and a main VM sequencer thread. */

#ifndef _LEAF_CHANNEL_H_
#define _LEAF_CHANNEL_H_

#include <stdio.h>
#include <leaf/leaf.h>
#include <leaf/port.h>


typedef struct { leaf_class super; } channel_class;

typedef struct {
    channel_class *type;
    leaf_object *object;
    int open;

    pthread_mutex_t mut;
    pthread_cond_t get_ok;
    pthread_cond_t put_ok;
    
} channel;

leaf_object *channel_get(channel *x);
int channel_put(channel *x, leaf_object *object);
int channel_get_would_block(channel *x);
int channel_put_would_block(channel *x);

channel_class *channel_type(void);
channel* channel_new(void);


/* The main usage for channel objects is to provide non-blocking
   behaviour for binary data conversion (parsing and printing).  These
   functions create channels that represent external parsing/printing
   running in a separate thread using blocking I/O on a file port. */

typedef leaf_object* (*port_reader)(void *ctx, port *p);
typedef int (*port_writer)(void *ctx, port *p, leaf_object *ob);

channel* channel_from_input_port(port *p, port_reader fn, void *ctx);
channel *channel_from_output_port(port *p, port_writer fn, void *ctx);




#endif
