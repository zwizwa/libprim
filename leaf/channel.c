#include <stdlib.h>
#include <leaf/leaf.h>
#include <leaf/channel.h>
#include <pthread.h>

static int channel_write(channel *x, port *p) {
    return port_printf(p, "#<channel>");
}
/* Note that a channel always has exactly two references.  

   FIXME: Teardown: one end closes the channel by setting a variable,
   while the other will properly cleanup the the data structures. */
static void channel_free(channel *x) {
    if (x->rc == 2) {
        fprintf(stderr, "RC=2 %p\n", x);
        x->rc--;
        pthread_cond_signal(&x->get_ok);
        pthread_cond_signal(&x->put_ok);
    }
    else {
        fprintf(stderr, "RC=1 %p\n", x);
        if (x->object) leaf_free((leaf_object*)x->object);
        free(x);
    }
}

LEAF_SIMPLE_TYPE(channel)

static void channel_init(channel *x) {
    x->type = channel_type();
    x->object = NULL;
    x->rc = 2;
    pthread_mutex_init(&x->mut, NULL);
    pthread_cond_init(&x->get_ok, NULL);
    pthread_cond_init(&x->put_ok, NULL);
}
channel *channel_new(void) {
    channel *x = calloc(1, sizeof(*x));
    channel_init(x);
    return x;
}
leaf_object *channel_get(channel *x) {
    leaf_object *ob = NULL;
    pthread_mutex_lock(&x->mut);
        while((!x->object) && (x->rc == 2)) { pthread_cond_wait(&x->get_ok, &x->mut); }
        if (x->rc == 2) {
            ob = x->object;
            x->object = NULL;
            pthread_cond_signal(&x->put_ok);  // wake up producer
        }
    pthread_mutex_unlock(&x->mut);
    return ob;
}
int channel_put(channel *x, leaf_object *ob) {
    int rv = -1;
    pthread_mutex_lock(&x->mut);
        while(x->object && (x->rc == 2)) { pthread_cond_wait(&x->put_ok, &x->mut); }
        if (x->rc == 2) {
            x->object = ob;
            pthread_cond_signal(&x->get_ok); // wake up consumer
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



/* Parsing/printing channels. */
typedef struct {
    channel chan;
    void *ctx;
    port *p;
    union {
        port_reader read;
        port_writer write;
        void *fn;
    } io;
    pthread_t thread; 
} io_channel;


//static void io_channel_free(io_channel *x) {
//    // pthread_destroy(&x->thread);
//    leaf_free((leaf_object*)x->p);
//    channel_free(&x->chan);
//}
static channel* channel_from_port(port *p, void *io_fn, void *ctx, void *thread_fn ) {
    io_channel *x = calloc(1, sizeof(*x));
    channel_init(&x->chan);
    x->ctx = ctx;
    x->p = p;
    x->io.fn = io_fn;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_create(&x->thread, &attr, thread_fn, x);
    return (channel*)x;
}

/* Note that channels transfer ownership. */
static void read_thread(io_channel *x) {
    leaf_object *o;
    for(;;) {
        if (!(o = x->io.read(x->ctx, x->p))) break;
        if (channel_put(&x->chan, o)) {
            leaf_free(o);
            break;
        }
    }
    leaf_free((leaf_object*)x);
}
static void write_thread(io_channel *x) {
    leaf_object *o;
    for(;;) {
        if (!(o = channel_get(&x->chan))) break;
        if (x->io.write(x->ctx, x->p, o)) {
            leaf_free(o);
            break;
        }
    }
    leaf_free((leaf_object*)x);
}
channel* channel_from_output_port(port *p, port_writer write, void *ctx) {
    return channel_from_port(p, write, ctx, write_thread);
}
channel* channel_from_input_port(port *p, port_reader read, void *ctx) {
    return channel_from_port(p, read, ctx, read_thread);
}