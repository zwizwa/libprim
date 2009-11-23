#include <leaf/grid.h>
#include <leaf/port.h>
#include <leaf/error.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static void grid_free(grid *x){
    free(x->buf);
    free(x);
}
#define FOR_DIM(i,x) for(i=0; i<x->rank; i++)
static int grid_write(grid *x, port *p) {
#if 1
    return grid_dump(x, p);
#else
    int i, len = 0;
    len += port_printf(p, "#<grid");
    FOR_DIM(i,x) len += port_printf(p, ":%d", x->dim[i]);
    len += port_printf(p, ">");
    return len;
#endif
}



LEAF_SIMPLE_TYPE(grid)

static grid *grid_proto(int rank) {
    grid *x = malloc(sizeof(*x) + (rank * sizeof(int*)));
    x->type = grid_type();
    x->rank = rank;
    x->buf = NULL;
    int i;
    FOR_DIM(i,x) x->dim[i] = 1;
    return x;
}
static void grid_alloc(grid *x, grid_atom init) {
    int i, total = grid_total(x);
    x->buf = malloc(sizeof(grid_atom) * grid_total(x));    
    for (i=0; i<total; i++) x->buf[i] = init;
}


int grid_total(grid *g) {
    int i, total = 1;
    FOR_DIM(i,g) total *= g->dim[i];
    return total;
}

grid *grid_new_1(int length, grid_atom init) {
    if (length < 1) return NULL;
    grid *x = grid_proto(1);
    x->dim[0] = length;
    grid_alloc(x, init);
    return x;
}
grid *grid_new_2(int d0, int d1, grid_atom init) {
    if ((d0 < 1) || (d1 < 1)) return NULL;
    grid *x = grid_proto(2);
    x->dim[0] = d0;
    x->dim[1] = d1;
    grid_alloc(x, init);
    return x;
}
grid *grid_copy(grid *template) {
    grid *x = grid_proto(template->rank);
    int i;
    FOR_DIM(i,x) x->dim[i] = template->dim[i];
    int nb = grid_total(template) * sizeof(grid_atom);
    x->buf = malloc(nb);
    memcpy(x->buf, template->buf, nb);
    return x;
}

// grid *grid_new_matrix(int rows, int columns);


typedef void (*fn_1)(grid_atom *);
typedef void (*fn_2)(grid_atom *, grid_atom *);
typedef void (*fn_3)(grid_atom *, grid_atom *, grid_atom *);
typedef void (*fn_4)(grid_atom *, grid_atom *, grid_atom *, grid_atom *);

#define MIN(a,b) ((a<b) ? (a) : (b))

/* Iterate over a grid.  The function is a Oz style procedure, where
   each argument is reference (pointer) to a grid_atom. */


int grid_for_each(grid_proc *p, int argc, grid **argv) {
    int i,j;
    int total = grid_total(argv[0]);
    if (p->argc != argc) return -1;
    for (j = 1; j < argc; j++) total = MIN(grid_total(argv[j]), total);

    switch(argc) {
    case 1: 
    {
        fn_1 f = (fn_1)p->fn;
        grid_atom *g0 = argv[0]->buf;
        for (i=0; i<total; i++) f(g0 + i);
        return 0;
    }
    case 2:
    {
        fn_2 f = (fn_2)p->fn;
        grid_atom *g0 = argv[0]->buf;
        grid_atom *g1 = argv[1]->buf;
        for (i=0; i<total; i++) f(g0 + i, g1 + i);
        return 0;
    }
    case 3:
    {
        fn_3 f = (fn_3)p->fn;
        grid_atom *g0 = argv[0]->buf;
        grid_atom *g1 = argv[1]->buf;
        grid_atom *g2 = argv[2]->buf;
        for (i=0; i<total; i++) f(g0 + i, g1 + i, g2 + i);
        return 0;
    }
    case 4:
    default:
        return -1;
    }
}



static void grid_proc_free(grid *x){
    free(x);
}
static int grid_proc_write(grid *x, port *p) {
    return port_printf(p, "#<grid-proc:%p>", x);
}

LEAF_SIMPLE_TYPE(grid_proc)

grid_proc *grid_proc_new(void *fn, int argc) {
    if (argc < 1) return NULL;
    
    grid_proc *x = calloc(1, sizeof(*x));
    x->type = grid_proc_type();
    x->fn = fn;
    x->argc = argc;
    return x;
}

int grid_dump(grid *g, port *p) {
    int len = 0, i,j,d;
    grid_atom *a = g->buf;
    len += port_printf(p, "\n#<grid:%d", g->dim[0]);
    for (d = 1; d<g->rank; d++) len+= port_printf(p, "x%d", g->dim[d]);

    // FIXME: only print vector + matrix (for other: first matrix plane)
    int jmax = 1;
    if (g->rank > 1) jmax = g->dim[1];

    for (j = 0; j<jmax; j++) {
        len += port_printf(p, "\n |");
        for(i = 0; i<g->dim[0]; i++) {
            len += port_printf(p, " %+f", *a++);
        }
    }
    len += port_printf(p, " >");
    return len;
}

/* Fill with noise from normal distribution. */
void grid_noise_normal(grid *g) {
    int i, N = grid_total(g);
    for (i = 0; i < N; i+=2) {
        float a = ((float)rand()) * (2.0f * M_PI * (1.0f / ((float)RAND_MAX)));
        float r = ((float)(1 + rand())) * (1.0f / ((float)RAND_MAX));
        float x = cos(a);
        float y = sin(a);
        r = sqrt(-2.0f * log(r));
        g->buf[i]   = x*r;
        if (i+1 < N) { g->buf[i+1] = y*r; }
    }
}


int grid_read_short(grid *g, port *p) {
    if (g->rank != 2) return -1;
    int chans = g->dim[0];
    int frames = g->dim[1];
    int i,j;
    grid_atom *a = g->buf;
    for (i=0; i<frames; i++) {
        short int sbuf[chans];
        port_read(p, sbuf, sizeof(short int) * chans);
        for(j=0; j<chans; j++) {
            *a++ = (double)sbuf[j];
        }
    }
    return 0;
}
int grid_write_short(grid *g, port *p) {
    if (g->rank != 2) return -1;
    int chans = g->dim[0];
    int frames = g->dim[1];
    int i,j;
    grid_atom *a = g->buf;
    for (i=0; i<frames; i++) {
        short int sbuf[chans];
        for(j=0; j<chans; j++) {
            double x = *a++;
            sbuf[j] = 
                (x > 0x7FFF) ? 0x7FFF :
                (x < -0x8000) ? -0x8000 : x;
        }
        port_write(p, sbuf, sizeof(short int) * chans);
    }
    return 0;
}
