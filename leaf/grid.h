#ifndef _LEAF_GRID_H_
#define _LEAF_GRID_H_

/* Grid object.  Currently, this contains only floats.  The idea is to
   make most operations generic: factor as iterators and iteratees. */

typedef float grid_atom;

#include <leaf/leaf.h>

typedef struct {
    leaf_class super;
} grid_class;

#define MAX_DIMS 4

typedef struct {
    grid_class *type;
    int dim[MAX_DIMS];
    grid_atom *buf;
} grid;



typedef struct {
    leaf_class super;
} grid_proc_class;

#define MAX_DIMS 4

typedef struct {
    grid_proc_class *type;
    void *fn;
    int argc;
} grid_proc;



grid_class *grid_type(void);
grid *grid_new_1(int length);
grid *grid_new_2(int rows, int columns);


grid_proc_class *grid_proc_type(void);
grid_proc *grid_proc_new(void *fn, int argc);
int grid_for_each(grid_proc *p, int argc, grid **argv);

int grid_dump(grid *g, port *p);


#define LEAF_SIMPLE_TYPE(name) \
    name##_class *name##_type(void) { \
    static name##_class *x = NULL; \
    if (!x) {\
    x = calloc(1, sizeof(*x)); \
    x->super.free = (leaf_free_m)name##_free; \
    x->super.write = (leaf_write_m)name##_write; \
    } return x; }




#endif
