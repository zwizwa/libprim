#include <media/gsl.h>

/* NOTES: the MV and MM operations _ACCUMULATE_ */


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


/* Simulate a state-space model, record its output. 

   [ x+ ]    [ A | B ] [ x ]
   [----]  = [---+---] [---]
   [ y  ]    [ C | D ] [ u ]

*/

#define MUL_MM(A,B,C,b) \
    gsl_blas_dgemm(CblasNoTrans, CblasNoTrans, 1.0, A, B, b, C)

#define MUL_MV(A,x,y,b) \
    gsl_blas_dgemm(CblasNoTrans, 1.0, A, x, b, y)

/* int grid_simulate(grid *gU, grid *gS, grid *gin, grid *gout) { */
/*     if (!((gU->rank = 2) && */
/*           (gS->rank = 1) && */
/*           (gin->rank = 2) && */
/*           (gout->rank = 2) && */
/*           (gin->dim[1] != gout->dim[1]))) return -1; */

/*     MATRIX(U, gU); */
/*     VECTOR(S, gS); */
/*     MATRIX(in, gin); */
/*     MTARIX(out, gout); */

/*     // dims */
/*     int N = gS->dim[0];   // state space */
/*     int I = gin->dim[0];  // input */
/*     int O = gout->dim[0]; // output */

/*     int T = gin->dim[1];  // nb of iterations */

/*     if (!((N+I == gS->dim[1]) && */
/*           (N+O == gS->dim[0]))) return -1; */

/*     int s = gU->dim[0];   // U stride */

/*     // create 4 component views of S */
/*     gsl_matrix_view A = gsl_matrix_submatrix(S, 0,0, N,N); */
/*     gsl_matrix_view B = gsl_matrix_submatrix(S, 0,N, I,N); */
/*     gsl_matrix_view C = gsl_matrix_submatrix(S, N,0, O,N); */
/*     gsl_matrix_view D = gsl_matrix_submatrix(S, N,N, O,I); */

/*     // create component views of IO */
/*     gsl_vector vin  = {I,1,gin->buf,NULL,0}; */
/*     gsl_vector vout = {O,1,gout->buf,NULL,0}; */
    
/*     int i; */
/*     for (i=0; i<T; T++) { */
/*         MUL_MV(A.matrix,S,S,0.0);  // state update */
/*         MUL_MV(A.matrix,S,S,0.0);  // state update */
/*     } */
/* } */


int grid_unfold(grid *gA, grid *gx, grid *gout) {
    MATRIX(A, gA);
    int N = gA->dim[0];
    if (!((gA->rank == 2) &&
          (gout->rank == 2) &&
          (N == gA->dim[1]) &&
          (N == gout->dim[0]))) return -1;
    int t,T = gout->dim[1];

    gsl_vector vin  = {N, 1, gx->buf, NULL, 0};
    gsl_vector vout = {N, 1, gout->buf, NULL, 0};

    gsl_blas_dgemv(CblasNoTrans, 1.0, &A, &vin, 0.0, &vout);
    vin.data = vout.data;
    vout.data += N;

    for (t=1; t<T; t++) {
        gsl_blas_dgemv(CblasNoTrans, 1.0, &A, &vin, 0.0, &vout);
        vin.data += N;
        vout.data += N;
    }
    return 0;
}
