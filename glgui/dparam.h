#ifndef _PARAMS_H_
#define _PARAMS_H_

#include <leaf/queue.h>

/* Synchronize array of parameters between two threads, serving
   updates at both ends. */
typedef float value;
typedef void (*dparam_handle_fn)(void *ctx, queue *in, int id);
struct dparam {
    int nb_par;
    value *prev;
    value *cur;
    queue *in, *out;

    /* Handler for data found in the queue other than DPARAM_HDR_ID
       This function can use only queue_read_consume() to access the
       current message. */
    dparam_handle_fn fn; void *ctx;
};

enum dparam_msg_id {
    dparam_msg_id_none         = 0,
    dparam_msg_id_param_update = 1,
    dparam_msg_id_array        = 2,
};

/* For param updates a sentinel is easier to use than a count in the
   header.  The sender doesn't know the length before iteration
   starts. */
#define DPARAM_PARAM_UPDATE_SENTINEL -1

struct dparam_hdr {
    int id;
};

struct dparam_par {
    int id; // -1 is sentinel
    value val;
};

struct dparam_arr {
    int id; // array identifier
    int nb_el;
};

struct dparam *dparam_new(int nb_par);
void dparam_connect(struct dparam *a, struct dparam *b);
void dparam_send(struct dparam *x);
void dparam_recv(struct dparam *x);

void dparam_set(struct dparam *x, int param, value value);

void dparam_send_array(struct dparam *x, int id, int nb, value *arr);

#endif
