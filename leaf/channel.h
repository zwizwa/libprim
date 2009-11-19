/* Synchronous channels for leaf object communication between system
   threads and a main VM sequencer thread. */

#ifndef _LEAF_CHANNEL_H_
#define _LEAF_CHANNEL_H_

#include <stdio.h>
#include <leaf/leaf.h>
#include <leaf/port.h>
#include <semaphore.h>


typedef struct { leaf_class super; } channel_class;

typedef struct {
    channel_class *type;
    leaf_object *object;
    sem_t sem;
    int open;
} channel;

leaf_object *channel_get(channel *x);
int channel_put(channel *x, leaf_object *object);
int channel_get_would_block(channel *x);
int channel_put_would_block(channel *x);

channel_class *channel_type(void);
channel* channel_new(void);

#endif
