#include <media/gsl.h>

static inline void gsl_vector_from_grid(gsl_vector *v, grid *g) {
    v->size   = g->dim[0];
    v->stride = 1;
    v->data   = g->buf;
    v->block  = NULL;
    v->owner  = 0;
}

static inline void gsl_matrix_from_grid(gsl_matrix *m, grid *g) {
    m->size1 = (g->rank < 2) ? 1 : g->dim[1];
    m->size2 = g->dim[0];
    m->tda   = g->dim[0];
    m->data  = g->buf;
    m->block = NULL;
    m->owner = 0;
}

#define MATRIX(A, gA) gsl_matrix A; gsl_matrix_from_grid(&A, gA)
#define VECTOR(A, gA) gsl_vector A; gsl_vector_from_grid(&A, gA)

static void error_handler (const char * reason, 
                           const char * file, 
                           int line, 
                           int gsl_errno) {
    fprintf(stderr, "%s:%s:%d:%d\n", reason, file, line, gsl_errno);
}
#define SET_HANDLER() gsl_set_error_handler(error_handler)


// gsl_linalg_SV_decomp (gsl_matrix * A, gsl_matrix * V, gsl_vector * S, gsl_vector * work)
int grid_linalg_SV_decomp(grid *gA, grid *gV, grid *gS, grid *gwork) {
    SET_HANDLER();
    MATRIX(A, gA);
    MATRIX(V, gV);
    VECTOR(S, gS);
    VECTOR(work, gwork);
    return gsl_linalg_SV_decomp(&A, &V, &S, &work);
}

int grid_linalg_SV_solve(grid *gU, grid *gV, grid *gS, grid *gb, grid *gx) {
    SET_HANDLER();
    MATRIX(U, gU);
    MATRIX(V, gV);
    VECTOR(S, gS);
    VECTOR(b, gb);
    VECTOR(x, gx);
    return gsl_linalg_SV_solve(&U, &V, &S, &b, &x);
}

// generic matrix vector multiplication
int grid_blas_dgemv(int transA, double alpha, grid *gA, grid *gx, double beta, grid *gy) {
    SET_HANDLER();
    MATRIX(A, gA);
    VECTOR(x, gx);
    VECTOR(y, gy);
    return gsl_blas_dgemv(transA ? CblasTrans : CblasNoTrans, alpha, &A, &x, beta, &y);
}


