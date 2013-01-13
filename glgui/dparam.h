#ifndef _PARAMS_H_
#define _PARAMS_H_

#include <leaf/queue.h>

/* Communication tools for real-time GUI.

   This is mostly for implementing audio/video processing parameter
   editors and monitors, but might be useful for other tools.  The
   communication mechanism is leaf/queue.h but could be any sequential
   binary stream like a set of unix pipes or a TCP socket.

   - Synchronize array of parameters between two threads, mutable on
     both sides.

   - Allow for user-defined one-way messages (tagged value arrays) */

#ifndef VALUE_TYPE
#define VALUE_TYPE float
typedef VALUE_TYPE value;
#endif

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

    /* Only the master side will propagate parameter changes that come
       over the queue.  This is to avoid update loops in case of
       simultaneous value changes. */
    bool propagate;

    /* Handler for data found in the queue other than dparam_msg_id
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
