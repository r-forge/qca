
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "lp_lib.h"
#include <stdio.h>


void lp_min(double *objective,          /* Objective function          */
            int const_count,            /* Number of constraints       */
            double *constraints,        /* Has extra element on front  */
            int bin_count,              /* Number of binary variables  */
            int *bin_vec,               /* Indices of bin. variables   */
            double *obj_val,            /* Objective function value    */
            double *solution,           /* Result of call              */
            int *status)                /* Holds return value          */
{

int i = 0, result;


double *const_ptr;

lprec *lp;


/*
** Make an empty lp with bin_count variables. If it fails, return.
*/
lp = make_lp ((int) 0, bin_count);

if (lp == (lprec *) NULL)
    return;

set_verbose (lp, 1);
set_minim (lp);

/*
** "Objective" is a vector. Set the objective function. Return on fail.
*/
result = set_obj_fn (lp, objective);
if (result == 0)
    return;

/* Put problem into "row mode" */
set_add_rowmode (lp, TRUE);

/*
** If there are any constraints, see if they're dense or regular.
*/
if ((int) const_count > 0) {
    
    /*
    ** If we're using regular constaints, point "constr_ptr" at the first one.
    */
    const_ptr = constraints;

    /*
    ** Add constraints, one at a time; then move constr_ptr up.
    */
    for (i = 0; i < (int) const_count; i++) {
        add_constraint (lp, const_ptr,
            (short) const_ptr[(int) (bin_count) + 1], 
                    const_ptr[(int) (bin_count) + 2]);
        const_ptr += (int) bin_count + 2;
    }
    
}

/* Take problem out of "row mode" */
set_add_rowmode (lp, FALSE);

/*
** Set binary variables.
*/
if (bin_count > 0) {
    for (i = 0; i < (int) bin_count; i++)
        set_binary (lp, (int) (bin_vec[i]), TRUE);
}

int scale = 0;

set_scaling (lp, scale);
*status = (int) solve (lp);

if ((int) *status != 0) {
    delete_lp (lp);
    return;
}


/*
** We've succeeded. Extract the objective function's value and
** the values of the variables.
*/

*obj_val = get_objective (lp);

get_variables (lp, solution);

/*
** Free up the memory and return.
*/

delete_lp (lp);

return;

}
