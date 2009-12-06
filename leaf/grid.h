#ifndef _LEAF_GRID_H_
#define _LEAF_GRID_H_

/* Grid object.  Currently, this contains only double floats to make
   it easier to couple to GSL.  */

typedef double grid_atom;

#include <leaf/leaf.h>

typedef struct {
    leaf_class super;
} grid_class;

typedef struct {
    leaf_object base;
    grid_atom *buf;
    int rank;    // tensor rank
    int dim[0];  // dimensionality of each index
} grid;



typedef struct {
    leaf_class super;
} grid_proc_class;

typedef struct {
    leaf_object base;
    void *fn;
    int argc;
} grid_proc;

int grid_total(grid *x);


leaf_class *grid_type(void);
grid *grid_new_1(int length, grid_atom init);
grid *grid_new_2(int columns, int rows, grid_atom init);
grid *grid_copy(grid *template);


leaf_class *grid_proc_type(void);
grid_proc *grid_proc_new(void *fn, int argc);
int grid_for_each(grid_proc *p, int argc, grid **argv);

int grid_dump(grid *g, port *p);

void grid_noise_normal(grid *g);
int grid_read_short(grid *g, port *p);
int grid_write_short(grid *g, port *p);

#endif
