void lp_min(double *objective,          /* Objective function          */
            int const_count,            /* Number of constraints       */
            double *constraints,        /* Has extra element on front  */
            int bin_count,              /* Number of binary variables  */
            int *bin_vec,               /* Indices of bin. variables   */
            double *obj_val,            /* Objective function value    */
            double *solution,           /* Result of call              */
            int *status);               /* Holds return value          */
            
