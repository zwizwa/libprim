#ifndef _PARAMS_H_
#define _PARAMS_H_

#include <leaf/queue.h>

/* Synchronize array of parameters between two threads, serving
   updates at both ends. */
struct dparam {
    int nb_par;
    float *old;
    float *new;
    queue *in, *out;
};

#define DPARAM_HDR_ID 1
#define DPARAM_SENTINEL -1
struct dparam_hdr {
    int id;
};

struct dparam_par {
    int id; // -1 is sentinel
    float val;
};





#endif
