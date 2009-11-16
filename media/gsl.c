#include <media/gsl.h>

static inline void gsl_vector_from_grid(gsl_vector *v, grid *g) {
    v->size   = g->dim[0];
    v->stride = 1;
    v->data   = g->buf;
    v->block  = NULL;
    v->owner  = 0;
}

static inline void gsl_matrix_from_grid(gsl_matrix *m, grid *g) {
    m->size1 = g->dim[1];
    m->size2 = g->dim[0];
    m->tda   = g->dim[0];
    m->data  = g->buf;
    m->block = NULL;
    m->owner = 0;
}

#define MATRIX(A, gA) gsl_matrix A; gsl_matrix_from_grid(&A, gA)
#define VECTOR(A, gA) gsl_vector A; gsl_vector_from_grid(&A, gA)

// gsl_linalg_SV_decomp (gsl_matrix * A, gsl_matrix * V, gsl_vector * S, gsl_vector * work)
int grid_linalg_SV_decomp(grid *gA, grid *gV, grid *gS, grid *gwork) {
    MATRIX(A, gA);
    MATRIX(V, gV);
    VECTOR(S, gS);
    VECTOR(work, gwork);
    return gsl_linalg_SV_decomp(&A, &V, &S, &work);
}
