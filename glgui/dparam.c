#include <glgui/dparam.h>
#include <stdlib.h>
#define ASSERT LEAF_ASSERT
#define LOG LEAF_LOG

struct dparam *dparam_new(int nb_par) {
    struct dparam *x = malloc(sizeof(*x) * nb_par);
    x->out = NULL;
    x->in = queue_new(30 * nb_par);
    x->nb_par = nb_par;
    x->cur  = malloc(sizeof(float) * nb_par);
    x->prev = malloc(sizeof(float) * nb_par);
    for (int i = 0; i<nb_par; i++) {
        x->cur[i] = 0.0f;
        x->prev[i] = 0.0f;
    }
    return x;
}

void dparam_connect(struct dparam *a, struct dparam *b) {
    a->out = b->in;
    b->out = a->in;
}

#define TRY(x) if (0 != (err = (x))) goto error;

void dparam_send(struct dparam *x) {
    struct queue *q = x->out;
    int err = 0;
    ASSERT(q);
    struct dparam_hdr hdr = { .id = DPARAM_HDR_ID };
    TRY(queue_write_open(q));
    TRY(queue_write_append(q, &hdr, sizeof(hdr)));
    int nb_queued = 0;
    for (int i = 0; i < x->nb_par; i++) {
        float f = x->cur[i];
        if (x->prev[i] != f) {
            x->prev[i] = f;
            struct dparam_par par = {.id = i, .val = f};
            TRY(queue_write_append(q, &par, sizeof(par)));
            nb_queued++;
            LOG("send %d %f", i, f);
        }
    }
    if (nb_queued) {
        /* Commit message.  Not calling this aborts. */
        struct dparam_par par = {.id = DPARAM_SENTINEL};
        TRY(queue_write_append(q, &par, sizeof(par)));
        queue_write_close(q);
    }
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
                x->cur[par.id] = par.val;
                LOG("recv %d %f", par.id, par.val);
            }
        }
        else {
            /* Plugin? */
        }

        /* Acknowledge */
        queue_read_close(q);
    }
}
