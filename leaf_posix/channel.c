#include <stdlib.h>
#include <leaf/leaf.h>
#include <leaf_posix/channel.h>
#include <pthread.h>

static int channel_write(channel *x, port *p) {
    return port_printf(p, "#<channel>");
}

void channel_register(channel *x) {
    pthread_mutex_lock(&x->mut);
    x->rc++;
    pthread_mutex_unlock(&x->mut);
}
void channel_unregister(channel *x) {
    // int rc;
    pthread_mutex_lock(&x->mut);
    // rc = 
    --(x->rc);
    pthread_cond_broadcast(&x->teardown_ok);
    pthread_mutex_unlock(&x->mut);
    // fprintf(stderr, "RC-- %d\n", rc);
}

static void channel_free(channel *x) {
    // set teardown condition
    pthread_mutex_lock(&x->mut);

    // wait for empty channel.
    while(x->object && (!x->teardown)) { pthread_cond_wait(&x->put_ok, &x->mut); }
    x->teardown = 1;
    x->rc--;

    // make sure they all notice
    pthread_cond_broadcast(&x->put_ok);
    pthread_cond_broadcast(&x->get_ok);

    // wait until they've disengaged
    while(x->rc) { pthread_cond_wait(&x->teardown_ok, &x->mut); }
    pthread_mutex_unlock(&x->mut);

    // cleanup
    // fprintf(stderr, "RC=0 %p\n", x);
    if (x->object) leaf_free((leaf_object*)x->object);
    free(x);
}

LEAF_SIMPLE_TYPE(channel)

static void channel_init(channel *x) {
    leaf_init(&x->base, channel_type());
    x->object = NULL;
    x->rc = 1;
    pthread_mutex_init(&x->mut, NULL);
    pthread_cond_init(&x->get_ok, NULL);
    pthread_cond_init(&x->put_ok, NULL);
    pthread_cond_init(&x->teardown_ok, NULL);
}
channel *channel_new(void) {
    channel *x = calloc(1, sizeof(*x));
    channel_init(x);
    return x;
}

/* Get object.  Caller owns object.  NULL is returned in case the channel is closed. */
leaf_object *channel_get(channel *x) {
    leaf_object *ob;
    pthread_mutex_lock(&x->mut);

    /* Wait until something happens: either an object arrives, or
       the channel gets closed. */
    while((!x->object) && (!x->teardown)) { pthread_cond_wait(&x->get_ok, &x->mut); }

    /* An object arrived.  Get it and signal producers the next
       object can be produced. */
    if (x->object) {
        ob = x->object;
        x->object = NULL;
        pthread_cond_broadcast(&x->put_ok);
    }
    /* Channel is closed.  Don't do anything. */
    else {
        ob = NULL;
    }
    pthread_mutex_unlock(&x->mut);
    return ob;
}


int channel_put(channel *x, leaf_object *ob) {
    int rv;
    pthread_mutex_lock(&x->mut);
    
    /* Wait until something happens: either an object can be sent or
       the channel gets closed. */
    while(x->object && (!x->teardown)) { pthread_cond_wait(&x->put_ok, &x->mut); }

    /* Channel is closed.  Release the object. */
    if (x->teardown) {
        leaf_free(ob);
        rv = -1;
    }
    /* An object can be sent.  Save it and signal consumers. */
    else {
        x->object = ob;
        pthread_cond_broadcast(&x->get_ok);
        rv = 0;
    }
    pthread_mutex_unlock(&x->mut);
    return rv;
}

int channel_get_would_block(channel *x) {
    return (x->object == NULL);
}
int channel_put_would_block(channel *x) {
    return (x->object != NULL);
}







/* Connect producer/consumer threads to channel. */

typedef struct {
    channel *chan;
    leaf_object *ctx;
    union {
        channel_producer produce;
        channel_consumer consume;
        void *fn;
    } io;
    pthread_t thread; 
} port_io;

static int channel_add_client(channel *chan, void *io_fn, leaf_object *ctx, void *thread_fn) {
    port_io *x = calloc(1, sizeof(*x));
    x->chan = chan;
    x->ctx = ctx;
    x->io.fn = io_fn;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&x->thread, &attr, thread_fn, x);
    return 0;
}

/* Note that channels transfer object ownership. */
static void produce_thread(port_io *x) {
    leaf_object *o = NULL;
    channel_register(x->chan);
    for (;;) {
        if (!(o = x->io.produce(x->ctx))) break;
        if (channel_put(x->chan, o)) break;
    };
    channel_unregister(x->chan);
    if (x->ctx) leaf_free(x->ctx);
    free(x);
    fprintf(stderr, "leaving producer %p\n", x);
}
static void consume_thread(port_io *x) {
    leaf_object *o = NULL;
    channel_register(x->chan);
    for (;;) {
        if (!(o = channel_get(x->chan))) break;
        if (x->io.consume(x->ctx, o)) break;
    }
    channel_unregister(x->chan);
    if (x->ctx) leaf_free(x->ctx);
    free(x);
    fprintf(stderr, "leaving consumer %p\n", x);
}
int channel_connect_consumer(channel *c, channel_consumer consume, leaf_object *ctx) {
    return channel_add_client(c, consume, ctx, consume_thread);
}
int channel_connect_producer(channel *c, channel_producer produce, leaf_object *ctx) {
    return channel_add_client(c, produce, ctx, produce_thread);
}
