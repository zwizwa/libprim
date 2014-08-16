#include <glgui/dparam.h>
#include <stdlib.h>
#define ASSERT LEAF_ASSERT
#define LOG LEAF_LOG

struct dparam *dparam_new(int nb_par) {
    struct dparam *x = calloc(nb_par, sizeof(*x));
    x->in  = queue_new(3000 * nb_par);
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
    if (!q) {
        // LOG("No out queue.");
        return;
    }
    ASSERT(q);
    struct dparam_hdr hdr = { .id = dparam_msg_id_param_update };
    /* Begin transaction */
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
            LOG("%p send %d %f", x, i, f);
        }
    }
    if (nb_queued) {
        /* Finalize transaction. */
        struct dparam_par par = {.id = DPARAM_PARAM_UPDATE_SENTINEL};
        TRY(queue_write_append(q, &par, sizeof(par)));
        queue_write_close(q);
    }
    else {
        /* Abort transaction.  If there is nothing to send we don't
           send a header either. */
        queue_write_abort(q);
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
        switch(hdr.id) {
        case dparam_msg_id_param_update:
            while (1) {
                struct dparam_par par;
                q_read(q, &par, sizeof(par));
                if (par.id == DPARAM_PARAM_UPDATE_SENTINEL) break;
                ASSERT(par.id >= 0);
                ASSERT(par.id < x->nb_par);
                x->cur[par.id] = par.val;
                if (!x->propagate) {
                    /* Changes that come in over the channel will not
                       be sent back.  If one side has this set and the
                       other side does not, it disambiguates
                       simultaneous edits. */
                    x->prev[par.id] = par.val;
                }
                LOG("%p recv %d %f", x, par.id, par.val);
            }
            break;
        case dparam_msg_id_array: {
            struct dparam_arr arr;
            q_read(q, &arr, sizeof(arr));
            for (int i = 0; i < arr.nb_el; i++) {
                value val;
                q_read(q, &val, sizeof(val));
            }
            break;
        }
        default:
            /* Other data is passed to abstract handler. */
            if (x->fn) {
                x->fn(x->ctx, q, hdr.id);
            }
            break;
        }

        /* Acknowledge */
        queue_read_close(q);
    }
}

void dparam_set(struct dparam *x, int param, value value) {
    if ((param < 0) || (param > x->nb_par)) return;
    x->cur[param] = value;
}


void dparam_send_array(struct dparam *x, int id, int nb, value *val) {
    struct queue *q = x->out;
    int err = 0;
    ASSERT(q);
    /* Main header */
    struct dparam_hdr hdr = { .id = dparam_msg_id_array };
    TRY(queue_write_open(q));
    TRY(queue_write_append(q, &hdr, sizeof(hdr)));
    /* Array header */
    struct dparam_arr arr = { .id = id, .nb_el = nb };
    TRY(queue_write_append(q, &arr, sizeof(arr)));
    /* Array body */
    TRY(queue_write_append(q, val, sizeof(*val) * nb));
    queue_write_close(q);
    return;
  error:
    // LOG("error %d sending array %d", err, id);
    return;
}
