#include <stdbool.h>
#include "super_rows.h"
#include "utils.h"

void find_models(
    const int pichart[],    // the PI chart (column major)
    const int pirows,       // number of minterms
    const int picols,       // number of PIs
    const bool allsol,      // all solutions
    const int k,            // level of complexity to search solutions at, equal to the solution minimum, from findmin()
    const double maxcomb,
    const bool firstmin,
    int **solutions,        // return pointer to the vector containing the solutions
    int *nr,                // number of rows for the returning "matrix"
    int *nc                 // number of cols for the returning "matrix"
);
