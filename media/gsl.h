/* Leaf bindings for the GNU Scientific Library

   Note that the GSL is GPL, not LGPL!.  If you include this code in
   the libprim build, the result will be covered under the GPL.
 */

#ifndef _LEAF_GSL_H_
#define _LEAF_GSL_H_

#include <gsl/gsl_math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_poly.h>
#include <leaf/grid.h>

/* Direct gsl_ -> grid_ adapters. */
int grid_linalg_SV_decomp(grid *A, grid *V, grid *S, grid *work); 
int grid_linalg_SV_solve(grid *gU, grid *gV, grid *gS, grid *gb, grid *gx);

int grid_blas_dgemv(int transA, double alpha, grid *gA, grid *gx, double beta, grid *gy);
int grid_unfold(grid *gA, grid *gx, grid *gout);

#endif
