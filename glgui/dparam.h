#ifndef _PARAMS_H_
#define _PARAMS_H_

#include <leaf/queue.h>

/* Synchronize array of parameters between two threads, serving
   updates at both ends.

   This also contains some other pure message functionality like
   sending/receiving raw float arrays.

   FIXME: Find a good way to organize this functionality.  It feels a
   bit ad-hoc. */

typedef float value;
enum dparam_msg_id {
    dparam_msg_id_none         = 0,
    dparam_msg_id_param_update = 1,
    dparam_msg_id_array        = 2,
};
typedef void (*dparam_handle_fn)(void *ctx, queue *in, enum dparam_msg_id id);
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

/* Fixed size float arrays use a count in the header since the size is
   known before sending. */
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
