#include <glgui/dparam.h>
#include <stdlib.h>
#define ASSERT LEAF_ASSERT
#define LOG LEAF_LOG

struct dparam *dparam_new(int nb_par) {
    struct dparam *x = malloc(sizeof(*x) * nb_par);
    x->nb_par = nb_par;
    x->new = malloc(sizeof(float) * nb_par);
    x->old = malloc(sizeof(float) * nb_par);
    for (int i; i<nb_par; i++) {
        x->new[i] = 0.0f;
        x->old[i] = 0.0f;
    }
    return x;
}
void dparam_pair_new(int nb_par, struct dparam **a, struct dparam **b) {
    /* FIXME: Pick some good sizes. */
    queue *a2b = queue_new(30 * nb_par);
    queue *b2a = queue_new(30 * nb_par);
    *a = dparam_new(nb_par);
    (*a)->in = b2a;
    (*a)->out = a2b;
    *b = dparam_new(nb_par);
    (*b)->in = a2b;
    (*b)->out = b2a;
}

#define TRY(x) if (0 != (err = (x))) goto error;

void dparam_send(struct dparam *x) {
    struct queue *q = x->out;
    int err = 0;
    ASSERT(q);
    struct dparam_hdr hdr = { .id = DPARAM_HDR_ID };
    TRY(queue_write_open(q));
    TRY(queue_write_append(q, &hdr, sizeof(hdr)));
    for (int i = 0; i < x->nb_par; i++) {
        float f = x->new[i];
        if (x->old[i] != f) {
            x->old[i] = f;
            struct dparam_par par = {.id = i, .val = f};
            TRY(queue_write_append(q, &par, sizeof(par)));
        }
    }
    queue_write_close(q);
    return;

  error:
    /* Other side is not reading. */
    // LOG("dparam_send: err %d", err);
    return;
}


static void q_read(queue *q, void *buf, int size) {
    int err;
    if (QUEUE_ERR_OK != (err = queue_read_consume(q, buf, size))) {
        LOG("q_read() -> %d", err);
        ASSERT(0);
    }
}
void dparam_recv(struct dparam *x) {
    queue *q = x->in;
    ASSERT(q);
    while(1) {
        /* Read until end. */
        if (QUEUE_ERR_OK != queue_read_open(q)) return;

        /* Check header */
        struct dparam_hdr hdr;
        q_read(q, &hdr, sizeof(hdr));
        if (hdr.id == DPARAM_HDR_ID) {
            while (1) {
                struct dparam_par par;
                q_read(q, &par, sizeof(par));
                if (par.id == DPARAM_SENTINEL) break;
                ASSERT(par.id >= 0);
                ASSERT(par.id < x->nb_par);
                x->new[par.id] = par.val;
            }
        }
        else {
            /* Plugin? */
        }

        /* Acknowledge */
        queue_read_close(q);
    }
}
