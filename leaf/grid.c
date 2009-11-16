#include <leaf/grid.h>
#include <leaf/port.h>
#include <stdlib.h>
#include <leaf/error.h>

static void grid_free(grid *x){
    free(x->buf);
    free(x);
}
#define FOR_DIM(i) for(i=0; i<MAX_DIMS; i++)
static int grid_write(grid *x, port *p) {
    int i, len = 0;
    len += port_printf(p, "#<grid");
    FOR_DIM(i) len += port_printf(p, ":%d", x->dim[i]);
    len += port_printf(p, ">");
    return len;
}


LEAF_SIMPLE_TYPE(grid)

grid *grid_new_1(int length) {
    if (length < 1) return NULL;
    grid *x = calloc(1, sizeof(*x));
    x->type = grid_type();
    int i;
    FOR_DIM(i) x->dim[i] = 1;

    x->dim[0] = length;
    x->buf = calloc(sizeof(grid_atom), length);
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

int grid_total(grid *g) {
    int i, total = 1;
    FOR_DIM(i) total *= g->dim[i];
    return total;
}

int grid_for_each(grid_proc *p, int argc, grid **argv) {
    int i,j;
    int total = 0;
    if (p->argc != argc) return -1;
    for (j = 0; j < argc; j++) total = MIN(grid_total(argv[j]), total);

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
    int len = 0, i, total = grid_total(g);
    for (i = 0; i < total; i++) {
        len += port_printf(p, "%f ", g->buf[i]);
    }
    len += port_printf(p, "\n");
    return len;
}

