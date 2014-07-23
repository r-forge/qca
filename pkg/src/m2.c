//Graph Based Agent implementation

# include <R.h>
# include <Rinternals.h>
# include <R_ext/Rdynload.h>

SEXP m2(SEXP goodbad, SEXP valents, SEXP mvector, SEXP mbase, SEXP t1g, SEXP t1b, SEXP rnofl) {
    
    int *pmvector, *pmbase, *prnofl; //, *pt1b, *pt1g
    SEXP t1gc, t1bc, t2g, t2b, temp, final, tempfinal, cr, tobj, tl; // cr is current.row; tobj is a temporary object
    int i, j, k, ln, nofconditions, lt1c, lt2, newl, counter, tcounter; //ln is line.number
    int *pt1gc, *pt1bc, *pt2g, *pt2b, *ptemp, *pfinal, *ptempfinal, *pcr, *ptobj, *ptl;
    
    
    PROTECT(goodbad);
    PROTECT(valents);
    
    SEXP usage = PROTECT(allocVector(VECSXP, 15));
    SET_VECTOR_ELT(usage, 0, mvector = coerceVector(mvector, INTSXP));
    SET_VECTOR_ELT(usage, 1, mbase = coerceVector(mbase, INTSXP));
    SET_VECTOR_ELT(usage, 2, t1g = coerceVector(t1g, INTSXP));
    SET_VECTOR_ELT(usage, 3, t1b = coerceVector(t1b, INTSXP));
    SET_VECTOR_ELT(usage, 4, rnofl = coerceVector(rnofl, INTSXP));
    SET_VECTOR_ELT(usage, 5, tl = allocVector(INTSXP, 1)); // total number of lines
    
    nofconditions = length(mbase);
    
    SET_VECTOR_ELT(usage, 6, temp = allocVector(INTSXP, nofconditions));
    
    prnofl = INTEGER(rnofl);
    ptl = INTEGER(tl);
    ptl[0] = 1;
    for (i = 0; i < nofconditions; i++) {
        ptl[0] = ptl[0]*prnofl[i];
    }
    
    SET_VECTOR_ELT(usage, 7, tempfinal = allocVector(INTSXP, ptl[0])); // this should be changed
    // perhaps finding a better approximation of the final number of implicants
    // so that it wouldn't be needed to pre-allocate the entire huge vector...!
    SET_VECTOR_ELT(usage, 8, cr = allocVector(INTSXP, nofconditions));
    
    pmvector = INTEGER(mvector);
    pmbase = INTEGER(mbase);
    // pt1g = INTEGER(t1g);
    // pt1b = INTEGER(t1b);
    ptemp = INTEGER(temp);
    ptempfinal = INTEGER(tempfinal);
    pcr = INTEGER(cr);
    
    ln = 2;
    
    counter = 0; // to introduce found numbers in the tempfinal vector
    
    while (ln < ptl[0]) {
        
        //temp <- rep(0, nofconditions)
        for (i = 0; i < nofconditions; i++) {
            ptemp[i] = 0;
        }
        
        // current.row <- getRow(realnoflevels, line.number)
        for (i = 0; i < nofconditions; i++) {
            pcr[i] = div(div(ln - 1, pmvector[i]).quot, prnofl[i]).rem;
        }
        
        // t1g.copy <- t1g
        SET_VECTOR_ELT(usage, 9, t1gc = t1g);
        pt1gc = INTEGER(t1g);
        
        // t1b.copy <- t1b
        SET_VECTOR_ELT(usage, 10, t1bc = t1b);
        pt1bc = INTEGER(t1b);
        
        for (i = 0; i < nofconditions; i++) {
            if (pcr[i] > 0) {
                SET_VECTOR_ELT(usage, 11, t2g = VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(goodbad, i), pcr[i] - 1), 0));
                pt2g = INTEGER(t2g);
                SET_VECTOR_ELT(usage, 12, t2b = VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(goodbad, i), pcr[i] - 1), 1));
                pt2b = INTEGER(t2b);
                
                // intersection code for the t1gc with t2g
                lt1c = length(t1gc);
                lt2 = length(t2g);
                newl = lt1c < lt2 ? lt1c : lt2;
                
                // create a temporary object of length newl
                SET_VECTOR_ELT(usage, 13, tobj = allocVector(INTSXP, newl));
                ptobj = INTEGER(tobj);
                
                // look for all elements in t1gc that are found in t2g
                j = 0;
                k = 0;
                tcounter = 0;
                while (j < lt1c && k < lt2) {
                    if (pt1gc[j] < pt2g[k]) {
                        j++;
                    }
                    else if (pt1gc[j] > pt2g[k]) {
                        k++;
                    }
                    else { // if (pt1gc[j] == pt2g[k])
                        ptobj[tcounter] = pt1gc[j];
                        tcounter++;
                        j++;
                        k++;
                    }
                }
                
                if (tcounter > 0) {
                    // re-create t1gc by copying the elements stored in tobj
                    SET_VECTOR_ELT(usage, 9, t1gc = allocVector(INTSXP, tcounter));
                    pt1gc = INTEGER(t1gc);
                    for (j = 0; j < tcounter; j++) {
                        pt1gc[j] = ptobj[j];
                    }
                    
                    
                    SET_VECTOR_ELT(usage, 13, tobj = coerceVector(VECTOR_ELT(valents, i), INTSXP));
                    ptobj = INTEGER(tobj);
                    ptemp[i] = ptobj[pcr[i] - 1];
                    
                    
                    // intersect t1bc with t2b
                    lt1c = length(t1bc);
                    lt2 = length(t2b);
                    newl = lt1c < lt2 ? lt1c : lt2;
                    
                    SET_VECTOR_ELT(usage, 13, tobj = allocVector(INTSXP, newl));
                    ptobj = INTEGER(tobj);
                    
                    j = 0;
                    k = 0;
                    tcounter = 0;
                    while (j < lt1c && k < lt2) {
                        if (pt1bc[j] < pt2b[k]) {
                            j++;
                        }
                        else if (pt1bc[j] > pt2b[k]) {
                            k++;
                        }
                        else { // if (pt1bc[j] == pt2b[k])
                            ptobj[tcounter] = pt1bc[j];
                            tcounter++;
                            j++;
                            k++;
                        }
                    }
                    
                    if (tcounter == 0) {
                        //Rprintf("bad set empty\n");
                        for (j = 0; j < nofconditions; j++) {
                            tcounter += ptemp[j]*pmbase[j];
                        }
                        
                        ptempfinal[counter] = tcounter + 1;
                        counter += 1;
                        break;
                    }
                    else {
                        // re-create t1bc for further use
                        SET_VECTOR_ELT(usage, 10, t1bc = allocVector(INTSXP, tcounter));
                        pt1bc = INTEGER(t1bc);
                        for (j = 0; j < tcounter; j++) {
                            pt1bc[j] = ptobj[j];
                        }
                    }
                }
                else {
                    break;
                }
                
            }
        }
        
        ln += pmvector[i == nofconditions ? i - 1 : i];
        
    }
    
    
    SET_VECTOR_ELT(usage, 14, final = allocVector(INTSXP, counter));
    pfinal = INTEGER(final);
    
    for (i = 0; i < counter; i++) {
        pfinal[i] = ptempfinal[i];
    }
    
    
    UNPROTECT(3);
    return(final);
}

