#include <stdbool.h>
#include <math.h>
#include "utils.h"

void CCubes(
    const int p_tt[],       // the truth table
    const int ttrows,       // number of rows in the truth table
    const int nconds,       // number of conditions in the data
    const double p_data[],  // calibrated dataset
    const int nrdata,       // number of rows in the raw data
    const bool allsol,      // all solution
    const bool rowdom,      // row dominance
    const bool minpin,      // minimal number of PIs
    const double picons,    // PI consistency
    const int pidepth,      // depth in number of conditions for each PI
    const int p_fsconds[],  // are conditions fuzzy?
    const int soldepth,     // depth in number of PIs for each solution
    const double solcons,
    const double solcov,
    const double maxcomb,
    const bool keeptrying,

    // pointers to save the results
    int **pichart,          // final PI chart
    int **implicants,       // final implicants in matrix form
    int **models,           // final solution models
    int *foundPI_,          // final number of PIs found
    int *solrows,           // number of rows for the solution matrix
    int *solcols,           // number of columns for the solution matrix
    bool *complex,          // signal if the returned solution is incomplete due to a too complex PI chart
    // int *solmin_,           // solution minima

    const bool firstmin
);
