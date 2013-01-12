#ifndef _PARAMS_H_
#define _PARAMS_H_

#include <leaf/queue.h>

/* Synchronize array of parameters between two threads, serving
   updates at both ends. */
typedef float value;
struct dparam {
    int nb_par;
    value *prev;
    value *cur;
    queue *in, *out;
};

#define DPARAM_HDR_ID 1
#define DPARAM_SENTINEL -1
struct dparam_hdr {
    int id;
};

struct dparam_par {
    int id; // -1 is sentinel
    value val;
};

struct dparam *dparam_new(int nb_par);
void dparam_connect(struct dparam *a, struct dparam *b);
void dparam_send(struct dparam *x);
void dparam_recv(struct dparam *x);


#endif
