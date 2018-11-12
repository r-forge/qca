/*
Copyright (c) 2018, Adrian Dusa
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, in whole or in part, are permitted provided that the
following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * The names of its contributors may NOT be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

# include <float.h>
# include <stdlib.h>
# include <R.h>
# include <Rinternals.h>
# include <Rmath.h>
# include <R_ext/Rdynload.h>
SEXP C_setDimnames(SEXP tt, SEXP dimnames) {
    setAttrib(tt, R_DimNamesSymbol, dimnames);  
    return(R_NilValue);
}
static R_INLINE Rboolean hasDimnames(SEXP matrix) {
    return !Rf_isNull(getAttrib(matrix, R_DimNamesSymbol));
}
static R_INLINE Rboolean hasRownames(SEXP matrix) {
    return hasDimnames(matrix) ? !Rf_isNull(VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 0)) : FALSE;
}
static R_INLINE Rboolean hasColnames(SEXP matrix) {
    return hasDimnames(matrix) ? !Rf_isNull(VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 1)) : FALSE;
}
SEXP C_setColnames(SEXP matrix, SEXP colnames) {
    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 1, colnames);
    if (hasRownames(matrix)) {
        SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 0));
    }
    setAttrib(matrix, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);
    return(R_NilValue);
}
SEXP C_setRownames(SEXP matrix, SEXP rownames) {
    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, rownames);
    if (hasColnames(matrix)) {
        SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 1));
    }
    setAttrib(matrix, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);
    return(R_NilValue);
}
static R_INLINE Rboolean getpos(SEXP list, const char *str) {
    SEXP names = getAttrib(list, R_NamesSymbol);
    int pos = -1;
    if (!Rf_isNull(names)) {
        for (int i = 0; i < length(list); i++) {
            if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
               pos = i;
               break;
            }
        }
    }
    return(pos);
}
static R_INLINE void rowDominance(int *p_pichart, int pirows, int *survcols, int *p_cols, int *p_ck) {
    int picols = *survcols;
    int colsums[picols];
    int sortcol[picols];
    int temp;
    for (int c = 0; c < picols; c++) {
        colsums[c] = 0;
        for (int r = 0; r < pirows; r++) {
            colsums[c] += p_pichart[c * pirows + r];
        }
        sortcol[c] = c;
    }
    for (int c1 = 0; c1 < picols; c1++) {
        for (int c2 = c1 + 1; c2 < picols; c2++) {
            if (colsums[sortcol[c1]] < colsums[sortcol[c2]]) {
                temp = sortcol[c1];
                sortcol[c1] = sortcol[c2];
                sortcol[c2] = temp;
            }
        }
    }
    for (int c1 = 0; c1 < picols; c1++) {
        if (p_cols[sortcol[c1]]) {
            for (int c2 = c1 + 1; c2 < picols; c2++) {
                if (p_cols[sortcol[c2]]) {
                    if (colsums[sortcol[c1]] > colsums[sortcol[c2]]) {
                        Rboolean itcovers = TRUE; 
                        int r = 0;
                        while (r < pirows && itcovers) {
                            if (p_pichart[sortcol[c2] * pirows + r]) {
                                itcovers = p_pichart[sortcol[c1] * pirows + r];
                            }
                            r++;
                        }
                        if (itcovers) {
                            p_cols[sortcol[c2]] = FALSE;
                            --(*survcols);
                        }
                    }
                }
            }
        }
    }
    if (*survcols < picols) {
        int s = 0;
        for (int c = 0; c < picols; c++) {
            if (p_cols[c]) {
                for (int r = 0; r < pirows; r++) {
                    p_pichart[s * pirows + r] = p_pichart[c * pirows + r];
                }
                p_ck[s] = p_ck[c];
                s++;
            }
        }
    }
}
static R_INLINE void superRows(int *p_matrix, int rows, int *survcols, int *p_cols) {
    int cols = *survcols;
    int colsums[cols];
    int sortcol[cols];
    int temp;
    for (int c = 0; c < cols; c++) {
        colsums[c] = 0;
        for (int r = 0; r < rows; r++) {
            colsums[c] += p_matrix[c * rows + r];
        }
        sortcol[c] = c;
    }
    for (int c1 = 0; c1 < cols; c1++) {
        for (int c2 = c1 + 1; c2 < cols; c2++) {
            if (colsums[sortcol[c1]] > colsums[sortcol[c2]]) {
                temp = sortcol[c1];
                sortcol[c1] = sortcol[c2];
                sortcol[c2] = temp;
            }
        }
    }
    for (int c1 = 0; c1 < cols - 1; c1++) {
        if (p_cols[sortcol[c1]]) {
            for (int c2 = c1 + 1; c2 < cols; c2++) {
                if (p_cols[sortcol[c2]]) {
                    if (colsums[sortcol[c1]] <= colsums[sortcol[c2]]) {
                        Rboolean cover = TRUE; 
                        int r = 0;
                        while (r < rows && cover) {
                            if (p_matrix[sortcol[c1] * rows + r]) {
                                cover = p_matrix[sortcol[c2] * rows + r];
                            }
                            r++;
                        }
                        if (cover) {
                            p_cols[sortcol[c2]] = FALSE;
                            --(*survcols);
                        }
                    }
                }
            }
        }
    }
    if (*survcols < cols) {
        int s = 0;
        for (int c = 0; c < cols; c++) {
            if (p_cols[c]) {
                for (int r = 0; r < rows; r++) {
                    p_matrix[s * rows + r] = p_matrix[c * rows + r];
                }
                s++;
            }
        }
    }
}
static R_INLINE void increment(int k, int *e, int *h, int nconds, int *tempk, int minval) {
    if (k == 1) {
        tempk[0] += 1;
    }
    else {
        if (*e < nconds - *h) {
            *h = 1;
            tempk[k - 1] += 1;
            *e = tempk[k - 1];
            if (tempk[k - 1] < minval) {
                tempk[k - 1] = minval;
                *e = minval;
            }
        }
        else {
            *e = tempk[k - *h - 1] + 1;
            *h += 1; 
            Rboolean under = TRUE;
            for (int j = 0; j < *h; j++) {
                under = under && (*e + j < minval);
                tempk[k - *h + j] = *e + j;
            }
            if (under) {
                *h = 1;
                tempk[k - *h] = minval;
                *e = minval;
            }
        }
    }
}
static R_INLINE void fillMatrix(int nrows, int ncols, int nofl[], int *matrix, int startrow, int cols[], int plus1) {
    int mbase[ncols];
    int orep[ncols];
    for (int c = 0; c < ncols; c++) {
        if (c == 0) {
            mbase[ncols - c - 1] = 1;
            orep[c] = 1;
        }
        else {
            mbase[ncols - c - 1] = mbase[ncols - c] * nofl[ncols - c];
            orep[c] = orep[c - 1] * nofl[c - 1];
        }
    }
    for (int c = 0; c < ncols; c++) {
        int lt = mbase[c] * nofl[c];
        for (int o = 0; o < orep[c]; o++) {
            for (int l = 0; l < nofl[c]; l++) {
                for (int i = 0; i < mbase[c]; i++) {
                    matrix[startrow + nrows * cols[c] + lt * o + mbase[c] * l + i] = l + plus1;
                }
            }
        }
    }
}
static R_INLINE void calculateRows(int ncols, int nofl[], int arrange, int maxprod, int *rows) {
    *rows = 0;
    int e, h, k, prod;
    if (arrange == 0) {
        *rows = 1;
        for (int c = 0; c < ncols; c++) {
            *rows *= nofl[c]; 
        }
    }
    else {
        for (k = 1; k <= maxprod; k++) {
            int tempk[k];
            int nck = 1;
            for (int i = 1; i <= k; i++) {
                nck *= ncols - (k - i);
                nck /=  i;
            }
            for (int i = 0; i < k; i++) {
                tempk[i] = i;
            }
            e = 0;
            h = k;
            for (int count = 0; count < nck; count++) {
                if (count > 0) {
                    increment(k, &e, &h, ncols, tempk, 0);
                }
                prod = 1;
                for (int c = 0; c < k; c++) {
                    prod *= (nofl[tempk[c]] - 1);
                }
                *rows += prod;
            }
        }
    }
}
static R_INLINE void generateMatrix(int nrows, int ncols, int nofl[], int arrange, int maxprod, SEXP matrix) {
    int *p_matrix = INTEGER(matrix);
    int e, h, k, prod;
    if (arrange == 0) {
        int cols[ncols];
        for (int c = 0; c < ncols; c++) {
            cols[c] = c;
        }
        fillMatrix(nrows, ncols, nofl, p_matrix, 0, cols, 0);
    }
    else { 
        for (int i = 0; i < nrows * ncols; i++) {
            p_matrix[i] = 0;
        }
        int startrow = 0;
        for (k = 1; k <= maxprod; k++) {
            int tempk[k];
            int nck;
            nck = 1;
            for (int i = 1; i <= k; i++) {
                nck *= ncols - (k - i);
                nck /=  i;
            }
            for (int i = 0; i < k; i++) {
                tempk[i] = i;
            }
            e = 0;
            h = k;
            for (int count = 0; count < nck; count++) {
                if (count > 0) {
                    increment(k, &e, &h, ncols, tempk, 0);
                }
                prod = 1;
                int colsk[k];
                int noflk[k];
                for (int c = 0; c < k; c++) {
                    prod *= (nofl[tempk[c]] - 1);
                    colsk[c] = tempk[c];
                    noflk[c] = nofl[tempk[c]] - 1;
                }
                fillMatrix(nrows, k, noflk, p_matrix, startrow, colsk, 1);
                startrow += prod;
            }
        }
    }
}
static R_INLINE SEXP simplify(SEXP pichart, int *survrows, int *survcols, int *mincols, Rboolean *search) {
    int *p_pichart = LOGICAL(pichart);
    SEXP out, rows, cols;
    SEXP usage = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(usage, 0, out = allocMatrix(LGLSXP, *survrows, *survcols));
    int *pout = LOGICAL(out);
    memset(pout, FALSE, *survrows * *survcols * sizeof(int));
    int pirows = nrows(pichart);
    int picols = ncols(pichart);
    SET_VECTOR_ELT(usage, 1, rows = allocVector(LGLSXP, pirows));
    SET_VECTOR_ELT(usage, 2, cols = allocVector(LGLSXP, picols));
    int *p_rows = LOGICAL(rows);
    int *p_cols = LOGICAL(cols);
    int temp;
    int rowsums[pirows];
    int colsums[picols];
    int sortrow[pirows];
    int sortcol[picols];
    for (int r = 0; r < pirows; r++) {
        p_rows[r] = TRUE;
        rowsums[r] = 0;
        sortrow[r] = r;
    }
    int c = 0;
    while (c < picols && *search) {
        colsums[c] = 0;
        for (int r = 0; r < pirows; r++) {
            colsums[c] += p_pichart[c * pirows + r];
            rowsums[r] += p_pichart[c * pirows + r];
        }
        p_cols[c] = colsums[c] > 0;
        if (!p_cols[c]) {
            --(*survcols);
        }
        sortcol[c] = c;
        if (colsums[c] == pirows) {
            *search = FALSE;
            *mincols += 1;
            *survcols = 0; 
        }
        c++;
    }
    if (*search) {
        for (int c1 = 0; c1 < picols; c1++) {
            for (int c2 = c1 + 1; c2 < picols; c2++) {
                if (colsums[sortcol[c1]] < colsums[sortcol[c2]]) {
                    temp = sortcol[c1];
                    sortcol[c1] = sortcol[c2];
                    sortcol[c2] = temp;
                }
            }
        }
        for (int c1 = 0; c1 < picols; c1++) {
            if (p_cols[sortcol[c1]]) {
                for (int c2 = c1 + 1; c2 < picols; c2++) {
                    if (p_cols[sortcol[c2]]) {
                        if (colsums[sortcol[c1]] > colsums[sortcol[c2]]) {
                            Rboolean itcovers = TRUE; 
                            int r = 0;
                            while (r < pirows && itcovers) {
                                if (p_pichart[sortcol[c2] * pirows + r]) {
                                    itcovers = p_pichart[sortcol[c1] * pirows + r];
                                }
                                r++;
                            }
                            if (itcovers) {
                                p_cols[sortcol[c2]] = FALSE;
                                --(*survcols);
                                for (int r = 0; r < pirows; r++) {
                                    rowsums[r] -= p_pichart[sortcol[c2] * pirows + r];
                                }
                            }
                        }
                    }
                }
            }
        }
        for (int r1 = 0; r1 < pirows; r1++) {
            for (int r2 = r1 + 1; r2 < pirows; r2++) {
                if (rowsums[sortrow[r1]] > rowsums[sortrow[r2]]) {
                    temp = sortrow[r1];
                    sortrow[r1] = sortrow[r2];
                    sortrow[r2] = temp;
                }
            }
        }
        for (int r1 = 0; r1 < pirows; r1++) {
            if (p_rows[sortrow[r1]]) {
                for (int r2 = r1 + 1; r2 < pirows; r2++) {
                    if (p_rows[sortrow[r2]]) {
                        if (rowsums[sortrow[r1]] < rowsums[sortrow[r2]]) {
                            int c = 0;
                            Rboolean iscovered = TRUE;
                            while (c < picols && iscovered) {
                                if (p_cols[c]) {
                                    if (p_pichart[c * pirows + sortrow[r1]]) {
                                        iscovered = p_pichart[c * pirows + sortrow[r2]];
                                    }
                                }
                                c++;
                            }
                            if (iscovered) {
                                p_rows[sortrow[r2]] = FALSE;
                                --(*survrows);
                            }
                        }
                    }
                }
            }
        }
        for (int c = 0; c < picols; c++) {
            if (p_cols[c]) {
                if (colsums[c] == 1) {
                    for (int r = 0; r < pirows; r++) {
                        if (p_pichart[c * pirows + r] && rowsums[r] == 1) {
                            p_rows[r] = FALSE;
                            p_cols[c] = FALSE;
                            rowsums[r] = pirows; 
                            colsums[c] = 0;
                            --(*survrows);
                            --(*survcols);
                            ++(*mincols);
                        }
                    }
                }
            }
        }
        SET_VECTOR_ELT(usage, 0, out = allocMatrix(LGLSXP, *survrows, *survcols));
        pout = LOGICAL(out);
        int col = 0;
        for (int c = 0; c < picols; c++) {
            if (p_cols[c]) {
                int row = 0;
                for (int r = 0; r < pirows; r++) {
                    if (p_rows[r]) {
                        pout[col * *survrows + row] = p_pichart[c * pirows + r];
                        row++;
                    }
                }
                col++;
            }
        }
    }
    UNPROTECT(1);
    return(out);
}
static R_INLINE SEXP transpose(SEXP matrix, int nr, int nc) {
    SEXPTYPE type = TYPEOF(matrix);
    SEXP out = PROTECT(allocMatrix(type, nc, nr));
    if (type == INTSXP) {
        int *p_out = INTEGER(out);
        int *p_matrix = INTEGER(matrix);
        for (int r = 0; r < nr; r++) {
            for (int c = 0; c < nc; c++) {
                p_out[r * nc + c] = p_matrix[c * nr + r];
            }
        }
    }
    else if (type == LGLSXP) {
        int *p_out = LOGICAL(out);
        int *p_matrix = LOGICAL(matrix);
        for (int r = 0; r < nr; r++) {
            for (int c = 0; c < nc; c++) {
                p_out[r * nc + c] = p_matrix[c * nr + r];
            }
        }
    }
    UNPROTECT(1);
    return(out);
}
static R_INLINE int getmin(SEXP pichart, int foundPI) {
    int pirows = nrows(pichart);
    SEXP basemat, coverage, estimat, temp1, temp2, tempcov;
    int *p_pichart, *p_basemat, *p_coverage, *p_estimat, *p_temp1, *p_temp2, *p_tempcov;
    SEXP usage = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(usage, 0, basemat = allocMatrix(LGLSXP, pirows, foundPI));
    SET_VECTOR_ELT(usage, 1, coverage = allocMatrix(LGLSXP, pirows, foundPI));
    p_basemat = LOGICAL(basemat);
    p_coverage = LOGICAL(coverage);
    p_pichart = LOGICAL(pichart);
    for (int i = 0; i < pirows * foundPI; i++) {
        p_basemat[i] = p_pichart[i];
    }
    int nofrows = pirows;
    int mincols = 0;
    int newmincols = 0;
    Rboolean allrows = TRUE;
    int r = 0;
    while (r < pirows && allrows) {
        Rboolean covered = FALSE;
        int c = 0;
        while (c < foundPI && !covered) {
            covered = p_basemat[c * pirows + r];
            c++;
        }
        allrows = covered;
        r++;
    }
    Rboolean search = TRUE;
    if (allrows) { 
        int survrows = pirows;
        int survcols = foundPI;
        Rboolean identical = FALSE;
        while (!identical) {
            SET_VECTOR_ELT(usage, 1, coverage = simplify(basemat, &survrows, &survcols, &mincols, &search));
            if (search && survrows > 0 && survcols > 0) {
                identical = (survrows == pirows) && (survcols == foundPI);
                if (!identical) {
                    SET_VECTOR_ELT(usage, 0, basemat = duplicate(coverage));
                    p_basemat = LOGICAL(basemat);
                    foundPI = survcols;
                    pirows = survrows;
                }
            }
            else {
                identical = TRUE; 
            }
        }
        if (survcols > 0 && search) { 
            newmincols = 1;
            int tocheck = survcols;
            int estimcheck = (survcols < 10000) ? 10000 : survcols * 2;
            SET_VECTOR_ELT(usage, 2, estimat = allocVector(INTSXP, survrows * survcols));
            SET_VECTOR_ELT(usage, 3, temp1 = allocVector(INTSXP, estimcheck * survrows));
            SET_VECTOR_ELT(usage, 4, temp2 = allocVector(INTSXP, estimcheck * survrows));
            SET_VECTOR_ELT(usage, 5, tempcov = allocVector(INTSXP, estimcheck * survrows));
            p_estimat = INTEGER(estimat); 
            p_temp1 = INTEGER(temp1);
            p_temp2 = INTEGER(temp2);
            p_tempcov = INTEGER(tempcov);
            SET_VECTOR_ELT(usage, 1, coverage = duplicate(basemat));
            p_coverage = LOGICAL(coverage);
            for (int c = 0; c < foundPI; c++) {
                p_estimat[c * survrows + 0] = c;
            }
            int sums[survcols];
            while (newmincols < nofrows && search) {
                int newcheck = 0;
                int tc = 0;
                while (tc < tocheck && search) {
                    int maxsum = 0;
                    int c = 0;
                    while (c < survcols && search) {
                        sums[c] = 0;
                        Rboolean same = FALSE;
                        int r = 0;
                        while (r < newmincols && !same) {
                            same = p_estimat[tc * survrows + r] == c;
                            r++;
                        }
                        if (!same) {
                            for (int r = 0; r < survrows; r++) {
                                sums[c] += 1 * (p_coverage[tc * survrows + r] || p_basemat[c * survrows + r]);
                            }
                            search = sums[c] != survrows;
                            if (sums[c] > maxsum) {
                                maxsum = sums[c];
                            }
                        }
                        c++;
                    }
                    if (search) {
                        for (int c = 0; c < survcols; c++) {
                            if (sums[c] >= maxsum - 1) {
                                for (int r = 0; r < newmincols; r++) {
                                    p_temp1[newcheck * survrows + r] = p_estimat[tc * survrows + r];
                                }
                                p_temp1[newcheck * survrows + newmincols] = c;
                                for (int r = 0; r < survrows; r++) {
                                    p_tempcov[newcheck * survrows + r] = 1 * (p_coverage[tc * survrows + r] || p_basemat[c * survrows + r]);
                                }
                                newcheck++;
                                if (newcheck == estimcheck) {
                                    estimcheck *= 2;
                                    SET_VECTOR_ELT(usage, 4, temp2 = duplicate(temp1));
                                    p_temp2 = INTEGER(temp2);
                                    SET_VECTOR_ELT(usage, 3, temp1 = allocMatrix(INTSXP, survrows, estimcheck));
                                    p_temp1 = INTEGER(temp1);
                                    for (int i = 0; i < newcheck * survrows; i++) {
                                        p_temp1[i] = p_temp2[i];
                                    }
                                    SET_VECTOR_ELT(usage, 4, temp2 = duplicate(tempcov));
                                    p_temp2 = INTEGER(temp2);
                                    SET_VECTOR_ELT(usage, 5, tempcov = allocMatrix(INTSXP, survrows, estimcheck));
                                    p_tempcov = INTEGER(tempcov);
                                    for (int i = 0; i < newcheck * survrows; i++) {
                                        p_tempcov[i] = p_temp2[i];
                                    }
                                }
                            }    
                        }
                    }
                    tc++;
                }
                if (search) {
                    tocheck = newcheck;
                    newcheck = 0;
                    SET_VECTOR_ELT(usage, 2, estimat = allocMatrix(INTSXP, survrows, tocheck));
                    p_estimat = INTEGER(estimat);
                    for (int i = 0; i < tocheck * survrows; i++) {
                        p_estimat[i] = p_temp1[i];
                    }
                    SET_VECTOR_ELT(usage, 1, coverage = allocMatrix(INTSXP, survrows, tocheck));
                    p_coverage = INTEGER(coverage);
                    for (int i = 0; i < tocheck * survrows; i++) {
                        p_coverage[i] = p_tempcov[i];
                    }
                }
                newmincols++;
            }
        }
    }
    int totsol = mincols + newmincols;
    if (totsol == 0 && allrows) {
        totsol = 1;
    }
    UNPROTECT(1);
    return(totsol);
}
static R_INLINE double consistency(SEXP x, int k, int tempk[], int val[], Rboolean fuzzy[]) {
    SEXP y;
    double *p_x, *p_y;
    SEXP usage = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(usage, 0, x = coerceVector(x, REALSXP));
    p_x = REAL(x); 
    int nrowsx = nrows(x);
    int nconds = ncols(x) - 1;
    SET_VECTOR_ELT(usage, 1, y = allocMatrix(REALSXP, nrowsx, k));
    p_y = REAL(y); 
    for (int c = 0; c < k; c++) {
        if (fuzzy[c]) {
            Rboolean negation = val[c] == 0;
            for (int r = 0; r < nrowsx; r++) {
                p_y[c * nrowsx + r] = negation ? (1 - p_x[tempk[c] * nrowsx + r]) : p_x[tempk[c] * nrowsx + r];
            }
        }
        else {
            for (int r = 0; r < nrowsx; r++) {
                p_y[c * nrowsx + r] = (p_x[tempk[c] * nrowsx + r] == val[c]) ? 1 : 0;
            }
        }
    }
    double pminx;
    double sumx = 0, sumxy = 0;
    for (int r = 0; r < nrowsx; r++) {
        pminx = 1;
        for (int c = 0; c < k; c++) {
            if (p_y[c * nrowsx + r] < pminx) {
                pminx = p_y[c * nrowsx + r];
            }
        }
        sumx += pminx;
        sumxy += ((pminx < p_x[nconds * nrowsx + r]) ? pminx: p_x[nconds * nrowsx + r]);
    }
    UNPROTECT(1);
    return(sumxy / sumx);
}
SEXP C_solveChart(SEXP pichart, SEXP allsol, SEXP vdepth) {
    int *p_indmat, *p_temp1, *p_temp2, *p_mintpis, *p_cols;
    SEXP usage, indmat, temp1, temp2, mintpis, cols;
    int *p_pichart = LOGICAL(pichart);
    usage = PROTECT(allocVector(VECSXP, 5));
    int pirows = nrows(pichart); 
    int picols = ncols(pichart); 
    int k = getmin(pichart, picols);
    int depth = INTEGER(coerceVector(vdepth, INTSXP))[0];
    if (depth > 0 && depth < k) depth = k;
    int solfound = 0;
    if (k == picols && solfound == 0) {
        SET_VECTOR_ELT(usage, 2, temp2 = allocMatrix(INTSXP, k, 1));
        p_temp2 = INTEGER(temp2);
        for (int i = 0; i < k; i++) {
            p_temp2[i] = i + 1;
        }
        solfound = 1;
    }
    if (solfound == 0) {
        if (LOGICAL(allsol)[0]) {
            SET_VECTOR_ELT(usage, 0, indmat = allocMatrix(INTSXP, picols, pirows));
            p_indmat = INTEGER(indmat);
            memset(p_indmat, 0, picols * pirows * sizeof(int));
            SET_VECTOR_ELT(usage, 3, mintpis = allocVector(INTSXP, pirows));
            p_mintpis = INTEGER(mintpis);
            memset(p_mintpis, 0, pirows * sizeof(int));
            for (int r = 0; r < pirows; r++) {
                for (int c = 0; c < picols; c++) {
                    if (p_pichart[c * pirows + r]) {
                         p_indmat[r * picols + p_mintpis[r]] = c;
                         p_mintpis[r]++;
                    }
                }
            }
            SET_VECTOR_ELT(usage, 1, temp1 = allocMatrix(INTSXP, picols, p_mintpis[0]));
            p_temp1 = INTEGER(temp1);
            memset(p_temp1, 0, p_mintpis[0] * picols * sizeof(int));
            for (int i = 0; i < p_mintpis[0]; i++) {
                p_temp1[i * picols + p_indmat[i]] = 1;
            }
            int temp1cols = p_mintpis[0];
            for (int i = 1; i < pirows; i++) {
                SET_VECTOR_ELT(usage, 2, temp2 = allocMatrix(INTSXP, picols, temp1cols * p_mintpis[i]));
                p_temp2 = INTEGER(temp2);
                for (int j = 0; j < p_mintpis[i]; j++) {
                    memcpy(&p_temp2[j * temp1cols * picols], p_temp1, temp1cols * picols * sizeof(int));
                    for (int tc = 0; tc < temp1cols; tc++) {
                        p_temp2[(j * temp1cols + tc) * picols + p_indmat[i * picols + j]] = 1;
                    }
                }
                int temp2cols = ncols(temp2);
                SET_VECTOR_ELT(usage, 4, cols = allocVector(LGLSXP, temp2cols));
                p_cols = LOGICAL(cols);
                memset(p_cols, TRUE, temp2cols * sizeof(int));
                int survcols = temp2cols;
                superRows(p_temp2, picols, &survcols, p_cols);
                SET_VECTOR_ELT(usage, 1, temp1 = allocMatrix(INTSXP, picols, survcols));
                p_temp1 = INTEGER(temp1);
                memcpy(p_temp1, p_temp2, picols * survcols * sizeof(int));
                temp1cols = survcols;
            }
            SET_VECTOR_ELT(usage, 2, temp2 = allocMatrix(INTSXP, picols, temp1cols));
            p_temp2 = INTEGER(temp2);
            memset(p_temp2, 0, picols * temp1cols * sizeof(int));
            SET_VECTOR_ELT(usage, 4, cols = allocVector(INTSXP, temp1cols));
            p_cols = INTEGER(cols);
            memset(p_cols, 0, temp1cols * sizeof(int));
            int maxr = 0;
            for (int c = 0; c < temp1cols; c++) {
                for (int r = 0; r < picols; r++) {
                    if (p_temp1[c * picols + r]) {
                        p_temp2[c * picols + p_cols[c]] = r + 1;
                        p_cols[c]++;
                    }
                    if (maxr < p_cols[c]) {
                        maxr = p_cols[c];
                    }
                }
            }
            SET_VECTOR_ELT(usage, 1, temp1 = allocMatrix(INTSXP, maxr, temp1cols));
            p_temp1 = INTEGER(temp1);
            for (int c = 0; c < temp1cols; c++) {
                for (int r = 0; r < maxr; r++) {
                    p_temp1[c * maxr + r] = p_temp2[c * picols + r];
                }
            }
            int temp;
            int order[temp1cols];
            for (int c = 0; c < temp1cols; c++) {
                order[c] = c;
            }
            for (int r = maxr - 1; r >= 0; r--) {
                for (int c1 = 0; c1 < temp1cols; c1++) {
                    for (int c2 = c1 + 1; c2 < temp1cols; c2++) {
                        if (p_temp1[order[c1] * maxr + r] > p_temp1[order[c2] * maxr + r]) {
                            temp = order[c2];
                            for (int i = c2; i > c1; i--) {
                                order[i] = order[i - 1];
                            }
                            order[c1] = temp;
                        }
                    }
                }
            }
            for (int c1 = 0; c1 < temp1cols; c1++) {
                for (int c2 = c1 + 1; c2 < temp1cols; c2++) {
                    if (p_cols[order[c1]] > p_cols[order[c2]]) {
                        temp = order[c2];
                        for (int i = c2; i > c1; i--) {
                            order[i] = order[i - 1];
                        }
                        order[c1] = temp;
                    }
                }
            }
            SET_VECTOR_ELT(usage, 2, temp2 = allocMatrix(INTSXP, maxr, temp1cols));
            p_temp2 = INTEGER(temp2);
            for (int c = 0; c < temp1cols; c++) {
                for (int r = 0; r < maxr; r++) {
                    p_temp2[c * maxr + r] = p_temp1[order[c] * maxr + r];
                }
            }
        }
        else {
            int estimsol = 10;
            SET_VECTOR_ELT(usage, 1, temp1 = allocMatrix(INTSXP, k, estimsol));
            p_temp1 = INTEGER(temp1);
            memset(p_temp1, 0, k * estimsol * sizeof(int));
            int tempk[k];
            for (int i = 0; i < k; i++) {
                tempk[i] = i; 
            }
            tempk[k - 1] -= 1; 
            int e = 0;
            int h = k;
            Rboolean last = (picols == k);
            while ((tempk[0] != picols - k) || last) {
                increment(k, &e, &h, picols + last, tempk, 0);
                last = FALSE;
                Rboolean allrows = TRUE;
                int r = 0;
                while (r < pirows && allrows) {
                    Rboolean covered = FALSE;
                    int c = 0;
                    while (c < k && !covered) {
                        covered = p_pichart[tempk[c] * pirows + r];
                        c++;
                    }
                    allrows = covered;
                    r++;
                }
                if (allrows) {
                    for (int c = 0; c < k; c++) {
                        p_temp1[solfound * k + c] = tempk[c] + 1;
                    }
                    solfound++;
                    if (solfound == estimsol) {
                        estimsol *= 2;
                        SET_VECTOR_ELT(usage, 2, temp2 = duplicate(temp1));
                        p_temp2 = INTEGER(temp2);
                        SET_VECTOR_ELT(usage, 1, temp1 = allocMatrix(INTSXP, k, estimsol));
                        p_temp1 = INTEGER(temp1);
                        memset(p_temp1, 0, k * estimsol * sizeof(int));
                        memcpy(p_temp1, p_temp2, k * solfound * sizeof(int));
                    }
                }
            }
            SET_VECTOR_ELT(usage, 2, temp2 = allocMatrix(INTSXP, k, solfound));
            p_temp2 = INTEGER(temp2);
            memcpy(p_temp2, p_temp1, k * solfound * sizeof(int));
        }
    }
    UNPROTECT(1);
    return(temp2);
}
static R_INLINE Rboolean altb(double a, double b) {
    return (b - a) > ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * DBL_EPSILON);
}
static R_INLINE Rboolean agteb(double a, double b) {
    return((a > b) || (fabs(a - b) <= DBL_EPSILON));
}
static R_INLINE Rboolean solCons(SEXP x, int k, int tempk[], SEXP pi, SEXP ck, SEXP indx, SEXP fuzzy, double solcons, double solcov) {
    int nrowsx = nrows(x);
    int nconds = ncols(x) - 1;
    int foundPI = nrows(pi);
    int *p_pi = INTEGER(pi);
    int *p_ck = INTEGER(ck);
    int *p_indx = INTEGER(indx);
    SEXP y, ymat;
    double *p_x, *p_y, *p_ymat;
    SEXP usage = PROTECT(allocVector(VECSXP, 3));
    SET_VECTOR_ELT(usage, 0, ymat = allocMatrix(REALSXP, nrowsx, k));
    p_ymat = REAL(ymat);
    SET_VECTOR_ELT(usage, 1, x = coerceVector(x, REALSXP));
    p_x = REAL(x); 
    double sumy = 0;
    for (int r = 0; r < nrowsx; r++) {
        sumy += p_x[nconds * nrowsx + r];
    }
    for (int i = 0; i < k; i++) { 
        int k2 = p_ck[tempk[i]];
        SET_VECTOR_ELT(usage, 2, y = allocMatrix(REALSXP, nrowsx, k2));
        p_y = REAL(y); 
        for (int c = 0; c < k2; c++) {
            int cindx = p_indx[c * foundPI + tempk[i]];
            int val = p_pi[cindx * foundPI + tempk[i]] - 1;
            if (LOGICAL(fuzzy)[cindx]) {
                Rboolean negation = val == 0;
                for (int r = 0; r < nrowsx; r++) {
                    p_y[c * nrowsx + r] = negation ? (1 - p_x[cindx * nrowsx + r]) : p_x[cindx * nrowsx + r];
                }
            }
            else {
                for (int r = 0; r < nrowsx; r++) {
                    p_y[c * nrowsx + r] = (p_x[cindx * nrowsx + r] == val) ? 1 : 0;
                }
            }
        }
        double pminx;
        for (int r = 0; r < nrowsx; r++) {
            pminx = 1;
            for (int c = 0; c < k2; c++) {
                if (p_y[c * nrowsx + r] < pminx) {
                    pminx = p_y[c * nrowsx + r];
                }
            }
            p_ymat[i * nrowsx + r] = pminx;
        }
    }
    double pmaxx;
    double sumx = 0, sumxy = 0;
    for (int r = 0; r < nrowsx; r++) {
        pmaxx = 0;
        for (int c = 0; c < k; c++) {
            if (p_ymat[c * nrowsx + r] > pmaxx) {
                pmaxx = p_ymat[c * nrowsx + r];
            }
        }
        sumx += pmaxx;
        sumxy += ((pmaxx < p_x[nconds * nrowsx + r]) ? pmaxx: p_x[nconds * nrowsx + r]);
    }
    UNPROTECT(1);
    return(agteb(sumxy / sumx, solcons) && agteb(sumxy / sumy, solcov)); 
}
static R_INLINE SEXP solveChartCons(SEXP pi, SEXP ck, SEXP indx, SEXP data, SEXP fuzzy, int maxk, double solcons, double solcov, Rboolean allsol, int soldepth) {
    int *p_sol, *p_cksol, *p_tempmat;
    int foundPI = nrows(pi);
    SEXP usage, sol, cksol, tempmat;
    usage = PROTECT(allocVector(VECSXP, 3));
    int k = 1;
    int estimsol = 10;
    SET_VECTOR_ELT(usage, 1, sol = allocMatrix(INTSXP, maxk, estimsol));
    SET_VECTOR_ELT(usage, 2, cksol = allocVector(INTSXP, estimsol));
    p_sol = INTEGER(sol);
    p_cksol = INTEGER(cksol);
    memset(p_sol, 0, maxk * estimsol * sizeof(int));
    int solfound = 0;
    int prevfound = 0;
    if (soldepth > 0) {
        if (soldepth < maxk) maxk = soldepth;
    }
    while (k <= maxk) {
        int tempk[k];
        for (int i = 0; i < k; i++) {
            tempk[i] = i; 
        }
        tempk[k - 1] -= 1; 
        int e = 0;
        int h = k;
        Rboolean last = (foundPI == k);
        while ((tempk[0] != foundPI - k) || last) {
            increment(k, &e, &h, foundPI + last, tempk, 0);
            last = FALSE;
            Rboolean nonred = TRUE;
            int i = 0;
            while (i < prevfound && nonred) {
                int sumeq = 0;
                int v = 0;
                while (sumeq == v && v < p_cksol[i]) {
                    for (int c = 0; c < k; c++) {
                        if (p_sol[i * maxk + v] == tempk[c]) {
                            sumeq++;
                        }
                    }
                    v++;
                }
                if (sumeq == v) { 
                    nonred = FALSE; 
                }
                i++;
            }
            if (nonred) {
                if (solCons(data, k, tempk, pi, ck, indx, fuzzy, solcons, solcov)) {
                    for (int c = 0; c < k; c++) {
                        p_sol[solfound * maxk + c] = tempk[c];
                    }
                    p_cksol[solfound] = k;
                    solfound++;
                    if (solfound == estimsol) {
                        estimsol *= 2;
                        int totlent = maxk * solfound;
                        SET_VECTOR_ELT(usage, 0, tempmat = duplicate(sol));
                        p_tempmat = INTEGER(tempmat);
                        SET_VECTOR_ELT(usage, 1, sol = allocMatrix(INTSXP, maxk, estimsol));
                        p_sol = INTEGER(sol);
                        memset(p_sol, 0, maxk * estimsol * sizeof(int));
                        for (int i = 0; i < totlent; i++) {
                            p_sol[i] = p_tempmat[i];
                        }
                        SET_VECTOR_ELT(usage, 0, tempmat = duplicate(cksol));
                        p_tempmat = INTEGER(tempmat);
                        SET_VECTOR_ELT(usage, 2, cksol = allocVector(INTSXP, estimsol));
                        p_cksol = INTEGER(cksol);
                        for (int i = 0; i < solfound; i++) {
                            p_cksol[i] = p_tempmat[i];
                        }
                    }
                }
            }
        }
        prevfound = solfound;
        k += 1;
    }
    if (solfound == 0) {
        UNPROTECT(1);
        return(R_NilValue);
    }
    else {
        int finalrows = p_cksol[solfound - 1];
        SET_VECTOR_ELT(usage, 0, tempmat = allocMatrix(INTSXP, finalrows, solfound));
        p_tempmat = INTEGER(tempmat);
        memset(p_tempmat, 0, finalrows * solfound * sizeof(int));
        for (int c = 0; c < solfound; c++) {
            for (int r = 0; r < p_cksol[c]; r++) {
                p_tempmat[c * finalrows + r] = p_sol[c * maxk + r] + 1;
            }
        }
        UNPROTECT(1);
        return(tempmat);
    }
}
static R_INLINE void sortmat(int *p_matrix, int *p_colindx, int *p_ck, int nconds, int foundPI) {
    for (int i = 0; i < foundPI; i++) {
        p_colindx[i] = i;
    }
    int temp;
    for (int i = nconds - 1; i >= 0; i--) {
        for (int c1 = 0; c1 < foundPI; c1++) {
            for (int c2 = c1 + 1; c2 < foundPI; c2++) {
                if (p_matrix[p_colindx[c1] * nconds + i] < p_matrix[p_colindx[c2] * nconds + i]) {
                    temp = p_colindx[c2];
                    for (int c3 = c2; c3 > c1; c3--) {
                        p_colindx[c3] = p_colindx[c3 - 1];
                    }
                    p_colindx[c1] = temp;
                }
            }
        }
        Rboolean nonzero = TRUE;
        int zeroidx = 0;
        while(zeroidx < foundPI && nonzero) {
            nonzero = p_matrix[p_colindx[zeroidx] * nconds + i];
            zeroidx++;
        }
        zeroidx--;
        for (int c1 = 0; c1 < zeroidx; c1++) {
            for (int c2 = c1 + 1; c2 < zeroidx; c2++) {
                if (p_matrix[p_colindx[c1] * nconds + i] > p_matrix[p_colindx[c2] * nconds + i]) {
                    temp = p_colindx[c2];
                    for (int c3 = c2; c3 > c1; c3--) {
                        p_colindx[c3] = p_colindx[c3 - 1];
                    }
                    p_colindx[c1] = temp;
                }
            }
        }
    }
    for (int c1 = 0; c1 < foundPI; c1++) {
        for (int c2 = c1 + 1; c2 < foundPI; c2++) {
            if (p_ck[p_colindx[c1]] > p_ck[p_colindx[c2]]) {
                temp = p_colindx[c2];
                for (int c3 = c2; c3 > c1; c3--) {
                    p_colindx[c3] = p_colindx[c3 - 1];
                }
                p_colindx[c1] = temp;
            }
        }
    }
}
SEXP C_ccubes(SEXP list) {
    int checkmin; 
    SEXP   posmat,    negmat,    pichart,    temp,    indx,    ck,    tempcpy,    result,    pic;
    int *p_posmat, *p_negmat, *p_pichart, *p_temp, *p_indx, *p_ck, *p_tempcpy, *p_result, *p_pic;
    SEXP tt;
    SEXP usage = PROTECT(allocVector(VECSXP, 10));
    SET_VECTOR_ELT(usage, 0, tt = coerceVector(VECTOR_ELT(list, 0), INTSXP));
    int *p_tt = INTEGER(tt);
    int ttrows = nrows(tt); 
    int nconds = ncols(tt) - 1; 
    int posrows = 0;
    for (int r = 0; r < ttrows; r++) {
        posrows += p_tt[nconds * ttrows + r];
    }
    int negrows = ttrows - posrows;
    SET_VECTOR_ELT(usage, 1, posmat = allocMatrix(INTSXP, posrows, nconds));
    p_posmat = INTEGER(posmat);
    int decpos[posrows];
    int decneg[(negrows > 0) ? negrows : 1];
    SET_VECTOR_ELT(usage, 2, negmat = allocMatrix(INTSXP, (negrows == 0) ? 1 : negrows, nconds));
    p_negmat = INTEGER(negmat);
    int rowpos = 0;
    int rowneg = 0;
    for (int r = 0; r < ttrows; r++) {
        if (p_tt[nconds * ttrows + r] == 1) { 
            for (int c = 0; c < nconds; c++) {
                p_posmat[c * posrows + rowpos] = p_tt[c * ttrows + r];
            }
            rowpos++;
        }
        else { 
            for (int c = 0; c < nconds; c++) {
                p_negmat[c * negrows + rowneg] = p_tt[c * ttrows + r];
            }
            rowneg++;
        }
    }
    int noflevels[nconds];
    for (int c = 0; c < nconds; c++) {
        noflevels[c] = 0; 
    }
    for (int c = 0; c < nconds; c++) {
        for (int r = 0; r < ttrows; r++) {
            if (noflevels[c] < p_tt[c * ttrows + r]) {
                noflevels[c] = p_tt[c * ttrows + r];
            }
        }
        noflevels[c] += 1; 
    }
    int foundPI = 0;
    int prevfoundPI = 0;
    int estimpi = 10;
    SET_VECTOR_ELT(usage, 3, pichart = allocMatrix(LGLSXP, posrows, estimpi));
    p_pichart = LOGICAL(pichart);
    memset(p_pichart, FALSE, posrows * estimpi * sizeof(int));
    SET_VECTOR_ELT(usage, 4, temp = allocMatrix(INTSXP, nconds, estimpi));
    p_temp = INTEGER(temp);
    memset(p_temp, 0, nconds * estimpi * sizeof(int));
    SET_VECTOR_ELT(usage, 6, ck = allocVector(INTSXP, estimpi));
    p_ck = INTEGER(ck);
    int posminpin = getpos(list, "min.pin");
    Rboolean minpin = (posminpin >= 0) ? (LOGICAL(VECTOR_ELT(list, posminpin))[0]) : FALSE;
    if (posminpin < 0 && length(list) > 1) {
        if (isLogical(VECTOR_ELT(list, 1))) {
            minpin = LOGICAL(VECTOR_ELT(list, 1))[0];
        }
    }
    int posdepth = getpos(list, "depth");
    int pidepth = nconds;
    int soldepth = 5; 
    if (posdepth >= 0) {
        pidepth = INTEGER(coerceVector(VECTOR_ELT(list, posdepth), INTSXP))[0];
        soldepth = INTEGER(coerceVector(VECTOR_ELT(list, posdepth), INTSXP))[1];
    }
    if (pidepth == 0 || nconds < pidepth) {
        pidepth = nconds;
    }
    SET_VECTOR_ELT(usage, 5, indx = allocMatrix(INTSXP, pidepth, estimpi));
    p_indx = INTEGER(indx);
    memset(p_indx, 0, pidepth * estimpi * sizeof(int));
    int pospicons = getpos(list, "pi.cons");
    double picons = (pospicons >= 0) ? (REAL(VECTOR_ELT(list, pospicons))[0]) : 0;
    int k = 1;
    Rboolean morePIfound = TRUE;
    Rboolean foundX = TRUE; 
    int minPIs = 0;
    int depthcol = 0; 
    int posdata = getpos(list, "data");
    int posfs = getpos(list, "fs");
    while (k <= pidepth && morePIfound && foundX) {
        morePIfound = FALSE; 
        int mbase[k];
        mbase[0] = 1; 
        int tempk[k];
        for (int i = 0; i < k; i++) {
            tempk[i] = i; 
        }
        tempk[k - 1] -= 1; 
        int e = 0;
        int h = k;
        Rboolean last = (nconds == k);
        while ((tempk[0] != nconds - k) || last) {
            increment(k, &e, &h, nconds + last, tempk, 0);
            last = FALSE;
            for (int c = 1; c < k; c++) {
                mbase[c] = mbase[c - 1] * noflevels[tempk[c - 1]];
            }
            for (int r = 0; r < posrows; r++) {
                decpos[r] = 0;
                for (int c = 0; c < k; c++) {
                    decpos[r] += p_posmat[tempk[c] * posrows + r] * mbase[c];
                }
            }
            for (int r = 0; r < negrows; r++) {
                decneg[r] = 0;
                for (int c = 0; c < k; c++) {
                    decneg[r] += p_negmat[tempk[c] * negrows + r] * mbase[c];
                }
            }
            int possiblePIrows[posrows];
            possiblePIrows[0] = 0; 
            Rboolean possiblePI[posrows];
            possiblePI[0] = TRUE; 
            int found = 1;
            for (int r = 1; r < posrows; r++) {
                int prev = 0;
                Rboolean unique = TRUE; 
                while (prev < found && unique) {
                    unique = decpos[possiblePIrows[prev]] != decpos[r];
                    prev += 1;
                }
                if (unique) {
                    possiblePIrows[found] = r;
                    possiblePI[found] = TRUE;
                    found += 1;
                }
            }
            int compare = found;
            if (picons > 0) {
                int val[k];
                Rboolean fuzzy[k];
                for (int i = 0; i < compare; i++) {
                    for (int c = 0; c < k; c++) {
                        val[c] = p_posmat[tempk[c] * posrows + possiblePIrows[i]];
                        fuzzy[c] = LOGICAL(VECTOR_ELT(list, posfs))[tempk[c]];
                    }
                    if (altb(consistency(VECTOR_ELT(list, posdata), k, tempk, val, fuzzy), picons)) {
                        possiblePI[i] = FALSE;
                        found -= 1;
                    }
                }
            }
            else if (negrows > 0) {
                for (int i = 0; i < compare; i++) {
                    int j = 0;
                    while (j < negrows && possiblePI[i]) {
                        if (decpos[possiblePIrows[i]] == decneg[j]) {
                            possiblePI[i] = FALSE;
                            found -= 1;
                        }
                        j += 1;
                    }
                }
            }
            if (found) { 
                int frows[found];
                int pos = 0;
                for (int i = 0; i < compare; i++) {
                    if (possiblePI[i]) {
                        frows[pos] = possiblePIrows[i];
                        pos += 1;
                    }
                }
                for (int f = 0; f < found; f++) {
                    int tempc[k];
                    for (int c = 0; c < k; c++) {
                        tempc[c] = p_posmat[tempk[c] * posrows + frows[f]] + 1;
                    }
                    Rboolean nonred = TRUE; 
                    if (prevfoundPI > 0) {
                        int i = 0;
                        while (i < prevfoundPI && nonred) {
                            int sumeq = 0;
                            int v = 0;
                            while (sumeq == v && v < p_ck[i]) {
                                for (int c = 0; c < k; c++) {
                                    if (p_indx[i * pidepth + v] == tempk[c] + 1) { 
                                        sumeq += (p_temp[i * nconds + p_indx[i * pidepth + v] - 1] == tempc[c]);
                                    }
                                }
                                v += 1;
                            }
                            if (sumeq == v) { 
                                nonred = FALSE; 
                            }
                            i += 1;
                        }
                    }
                    if (nonred) { 
                        for (int c = 0; c < k; c++) {
                            p_temp[nconds * foundPI + tempk[c]] = tempc[c];
                        }
                        for (int c = 0; c < k; c++) {
                            p_indx[pidepth * foundPI + c] = tempk[c] + 1; 
                        }
                        p_ck[foundPI] = k;
                        for (int r = 0; r < posrows; r++) {
                            p_pichart[posrows * foundPI + r] = decpos[r] == decpos[frows[f]];
                        }
                        ++foundPI;
                        morePIfound = TRUE;
                        if (foundPI == estimpi) {
                            estimpi *= 2;
                            int totlent = nconds * foundPI;
                            SET_VECTOR_ELT(usage, 7, tempcpy = duplicate(temp));
                            p_tempcpy = INTEGER(tempcpy);
                            SET_VECTOR_ELT(usage, 4, temp = allocMatrix(INTSXP, nconds, estimpi));
                            p_temp = INTEGER(temp);
                            memset(p_temp, 0, nconds * estimpi * sizeof(int));
                            for (int i = 0; i < totlent; i++) {
                                p_temp[i] = p_tempcpy[i];
                            }
                            int totleni = pidepth * foundPI;
                            SET_VECTOR_ELT(usage, 7, tempcpy = duplicate(indx));
                            p_tempcpy = INTEGER(tempcpy);
                            SET_VECTOR_ELT(usage, 5, indx = allocVector(INTSXP, pidepth * estimpi));
                            p_indx = INTEGER(indx);
                            memset(p_indx, 0, pidepth * estimpi * sizeof(int));
                            for (int i = 0; i < totleni; i++) {
                                p_indx[i] = p_tempcpy[i];
                            }
                            SET_VECTOR_ELT(usage, 7, tempcpy = duplicate(ck));
                            p_tempcpy = INTEGER(tempcpy);
                            SET_VECTOR_ELT(usage, 6, ck = allocVector(INTSXP, estimpi));
                            p_ck = INTEGER(ck);
                            for (int i = 0; i < foundPI; i++) {
                                p_ck[i] = p_tempcpy[i];
                            }
                            SET_VECTOR_ELT(usage, 7, tempcpy = duplicate(pichart));
                            p_tempcpy = LOGICAL(tempcpy);
                            totlent = posrows * foundPI;
                            SET_VECTOR_ELT(usage, 3, pichart = allocMatrix(LGLSXP, posrows, estimpi));
                            p_pichart = LOGICAL(pichart);
                            memset(p_pichart, FALSE, posrows * estimpi * sizeof(int));
                            for (int i = 0; i < totlent; i++) {
                                p_pichart[i] = p_tempcpy[i];
                            }
                        }
                    }
                }
            }
        }
        if (foundPI > prevfoundPI) {
            depthcol = prevfoundPI + 1;
        }
        if (foundPI > 0) {
            checkmin = getmin(pichart, foundPI);
            if (checkmin > 0) { 
                if (minpin) {
                    if (checkmin == minPIs && morePIfound) {
                        if (nrows(pichart) > 1) {
                            int k2 = minPIs;
                            int tempk2[k2];
                            for (int i = 0; i < k2; i++) {
                                tempk2[i] = i;
                            }
                            tempk2[k2 - 1] -= 1;
                            int e2 = 0;
                            int h2 = k2;
                            Rboolean foundminpairs = FALSE;
                            Rboolean last = (foundPI == k2);
                            while (((tempk2[0] != foundPI - k2) || last) && !foundminpairs) {
                                increment(k2, &e2, &h2, foundPI + last, tempk2, depthcol);
                                last = FALSE;
                                Rboolean allrows = TRUE;
                                int r = 0;
                                while (r < posrows && allrows) {
                                    Rboolean covered = FALSE;
                                    int c = 0;
                                    while (c < k2 && !covered) {
                                        covered = p_pichart[tempk2[c] * posrows + r];
                                        c++;
                                    }
                                    allrows = covered;
                                    r++;
                                }
                                foundminpairs = allrows;
                            }
                            foundX = foundminpairs;
                        }
                        else { 
                            int i = depthcol;
                            while (i < foundPI && !foundX) {
                                foundX = p_pichart[i]; 
                                ++i;
                            }
                        }
                    }
                    minPIs = checkmin;
                }
            }
            else {
                morePIfound = TRUE;
            }
        }
        else {
            morePIfound = TRUE;
        }
        if (foundX) { 
            prevfoundPI = foundPI;
        }
        else {
            foundPI = prevfoundPI;
        }
        k += 1;
    }
    SEXP dimnames, ttcolnms,  colnms;
    SET_VECTOR_ELT(usage, 6, dimnames = allocVector(VECSXP, 2));
    if (hasColnames(tt)) {
        SET_VECTOR_ELT(usage, 7, ttcolnms = VECTOR_ELT(getAttrib(tt, R_DimNamesSymbol), 1));
        SET_VECTOR_ELT(usage, 8, colnms = allocVector(STRSXP, nconds));
        for (int i = 0; i < nconds; i++) {
            SET_STRING_ELT(colnms, i, STRING_ELT(ttcolnms, i));
        }
        SET_VECTOR_ELT(dimnames, 1, colnms); 
    }
    int posallsol = getpos(list, "all.sol");
    if (posallsol >= 0) { 
        int posrowdom = getpos(list, "row.dom");
        Rboolean rowdom = (posrowdom >= 0) ? (LOGICAL(VECTOR_ELT(list, posrowdom))[0]) : FALSE;
        SEXP out = PROTECT(allocVector(VECSXP, 3));
        SEXP cols;
        SET_VECTOR_ELT(usage, 7, cols = allocVector(LGLSXP, foundPI));
        int *p_cols = INTEGER(cols);
        memset(p_cols, TRUE, foundPI * sizeof(int));
        if (rowdom) { 
            int survcols = foundPI;
            rowDominance(p_pichart, posrows, &survcols, p_cols, p_ck);
            if (survcols < foundPI) {
                int s = 0;
                for (int c = 0; c < foundPI; c++) {
                    if (p_cols[c]) {
                        for (int r = 0; r < nconds; r++) {
                            p_temp[s * nconds + r] = p_temp[c * nconds + r];
                        }
                        s++;
                    }
                }
                foundPI = survcols;
            }
        }
        SET_VECTOR_ELT(usage, 8, tempcpy = allocVector(INTSXP, foundPI));
        p_tempcpy = INTEGER(tempcpy);
        sortmat(p_temp, p_tempcpy, p_ck, nconds, foundPI);
        SET_VECTOR_ELT(out, 0, result = allocMatrix(INTSXP, foundPI, nconds));
        p_result = INTEGER(result);
        SET_VECTOR_ELT(out, 1, pic = allocMatrix(LGLSXP, posrows, foundPI));
        p_pic = LOGICAL(pic);
        for (int c = 0; c < foundPI; c++) {
            for (int r = 0; r < posrows; r++) {
                p_pic[c * posrows + r] = p_pichart[p_tempcpy[c] * posrows + r];
            }
            for (int r = 0; r < nconds; r++) {
                p_result[foundPI * r + c] = p_temp[p_tempcpy[c] * nconds + r];
            }
        }
        if (hasColnames(tt)) {
            setAttrib(result, R_DimNamesSymbol, dimnames);  
        }
        int posolcons = getpos(list, "sol.cons");
        int posolcov  = getpos(list, "sol.cov");
        if (REAL(VECTOR_ELT(list, posolcons))[0] > 0) { 
            SEXP temptemp;
            SET_VECTOR_ELT(usage, 1, temptemp = duplicate(ck)); 
            int *p_temptemp = INTEGER(temptemp);
            SET_VECTOR_ELT(usage, 6, ck = allocVector(INTSXP, foundPI));
            p_ck = INTEGER(ck);
            for (int c = 0; c < foundPI; c++) {
                p_ck[c] = p_temptemp[p_tempcpy[c]];
            }
            SET_VECTOR_ELT(usage, 1, temptemp = duplicate(indx)); 
            p_temptemp = INTEGER(temptemp);
            SET_VECTOR_ELT(usage, 5, indx = allocMatrix(INTSXP, foundPI, pidepth));
            p_indx = INTEGER(indx);
            for (int r = 0; r < foundPI; r++) {
                for (int c = 0; c < pidepth; c++) {
                    p_indx[c * foundPI + r] = p_temptemp[p_tempcpy[r] * pidepth + c] - 1;
                }
            }
            SET_VECTOR_ELT(out, 2,
                solveChartCons(result, 
                               ck, 
                               indx, 
                               VECTOR_ELT(list, posdata), 
                               VECTOR_ELT(list, posfs), 
                               ((nrows(pic) < ncols(pic)) ? nrows(pic) : ncols(pic)), 
                               REAL(VECTOR_ELT(list, posolcons))[0], 
                               REAL(VECTOR_ELT(list, posolcov))[0], 
                               LOGICAL(VECTOR_ELT(list, posallsol))[0], 
                               soldepth
                              )
                          );
        }
        else {
            if (picons > 0 && getmin(pic, foundPI) == 0) {
                SET_VECTOR_ELT(out, 2, R_NilValue);
            }
            else {
                INTEGER(VECTOR_ELT(list, posdepth))[0] = INTEGER(VECTOR_ELT(list, posdepth))[1];
                SET_VECTOR_ELT(out, 2, C_solveChart(pic, VECTOR_ELT(list, posallsol), VECTOR_ELT(list, posdepth)));
            }
        }
        SET_VECTOR_ELT(out, 1, pic = transpose(pic, posrows, foundPI));
        UNPROTECT(2);
        return(out);
    }
    else {    
        SET_VECTOR_ELT(usage, 6, result = transpose(temp, nconds, foundPI));
        if (hasColnames(tt)) {
            setAttrib(result, R_DimNamesSymbol, dimnames);  
        }
        UNPROTECT(1);
        return(result);
    }
}
SEXP C_findmin(SEXP pichart) {
    SEXP out = PROTECT(allocVector(INTSXP, 1));
    INTEGER(out)[0] = getmin(pichart, ncols(pichart));
    UNPROTECT(1);
    return(out);
}
SEXP C_getRow(SEXP input) {
    PROTECT(input);
    SEXP rowno, noflevels, mbase, matrix;
    SEXP usage = PROTECT(allocVector(VECSXP, 4));
    SET_VECTOR_ELT(usage, 0, rowno = coerceVector(VECTOR_ELT(input, 0), INTSXP));
    SET_VECTOR_ELT(usage, 1, noflevels = coerceVector(VECTOR_ELT(input, 1), INTSXP));
    SET_VECTOR_ELT(usage, 2, mbase   = coerceVector(VECTOR_ELT(input, 2), INTSXP));
    int *p_rowno = INTEGER(rowno);
    int *p_noflevels = INTEGER(noflevels);
    int *p_mbase = INTEGER(mbase);
    int nrows = length(rowno);
    int ncols = length(noflevels);
    SET_VECTOR_ELT(usage, 3, matrix = allocMatrix(INTSXP, nrows, ncols));
    int *p_matrix = INTEGER(matrix);
    for (int r = 0; r < nrows; r++) {
        for (int c = 0; c < ncols; c++) {
            p_matrix[c * nrows + r] = (p_rowno[r] / p_mbase[c]) % p_noflevels[c];
        }
    }
    UNPROTECT(2);
    return(matrix);
}
SEXP C_createMatrix(SEXP input) {
    PROTECT(input);
    SEXP matrix, noflevels, arrange, maxprod;
    SEXP usage = PROTECT(allocVector(VECSXP, 4));
    SET_VECTOR_ELT(usage, 0, noflevels = coerceVector(VECTOR_ELT(input, 0), INTSXP));
    SET_VECTOR_ELT(usage, 1, arrange   = coerceVector(VECTOR_ELT(input, 1), INTSXP));
    SET_VECTOR_ELT(usage, 2, maxprod   = coerceVector(VECTOR_ELT(input, 2), INTSXP));
    int *p_noflevels = INTEGER(noflevels);
    int *p_arrange = INTEGER(arrange);
    int *p_maxprod = INTEGER(maxprod);
    int ncols = length(noflevels);
    int nofl[ncols];
    for (int c = 0; c < ncols; c++) {
        nofl[c] = p_noflevels[c];
    }
    if (p_maxprod[0] > ncols) {
        p_maxprod[0] = ncols;
    }
    int intarrange = p_arrange[0];
    int intmaxprod = p_maxprod[0];
    int nrows;
    calculateRows(ncols, nofl, intarrange, intmaxprod, &nrows);
    SET_VECTOR_ELT(usage, 3, matrix = allocMatrix(INTSXP, nrows, ncols));
    generateMatrix(nrows, ncols, nofl, intarrange, intmaxprod, matrix);
    if (length(input) > 3) { 
        SEXP dimnames;
        SET_VECTOR_ELT(usage, 3, dimnames = allocVector(VECSXP, 2));
        SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(input, 3)); 
        setAttrib(matrix, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(2);
    return(matrix);
}
SEXP C_superSubset(SEXP x, SEXP noflevels, SEXP fuz, SEXP vo,
                 SEXP nec, SEXP inclcut, SEXP covcut, SEXP depth) {
    SEXP usage = PROTECT(allocVector(VECSXP, 19));
    SET_VECTOR_ELT(usage,  0, x         = coerceVector(x, REALSXP));
    SET_VECTOR_ELT(usage,  1, noflevels = coerceVector(noflevels, INTSXP));
    SET_VECTOR_ELT(usage,  2, fuz       = coerceVector(fuz, INTSXP));
    SET_VECTOR_ELT(usage,  3, vo        = coerceVector(vo, REALSXP));
    SET_VECTOR_ELT(usage,  4, nec       = coerceVector(nec, INTSXP));
    SET_VECTOR_ELT(usage,  5, inclcut   = coerceVector(inclcut, REALSXP));
    SET_VECTOR_ELT(usage,  6, covcut    = coerceVector(covcut, REALSXP));
    SET_VECTOR_ELT(usage,  7, depth     = coerceVector(depth, INTSXP));
    double *p_x = REAL(x);
    int *p_noflevels = INTEGER(noflevels);
    int *p_fuz = INTEGER(fuz);
    double *p_vo = REAL(vo);
    int *p_nec = INTEGER(nec);
    double *p_inclcut = REAL(inclcut);
    double *p_covcut = REAL(covcut);
    int *p_depth = INTEGER(depth);
    int estim1 = 1000;
    int estim2 = 1000; 
    int xrows = nrows(x);
    int xcols = ncols(x);
    int nconds = ncols(x); 
    if (p_depth[0] == 0) {
        p_depth[0] = nconds;
    }
    SEXP tmconj, tmdisj, ticpr_conj, ticpr_disj, combkl, tcoms_conj, tcoms_disj,
         indx_conj, indx_disj, ck_conj, ck_disj;
    SET_VECTOR_ELT(usage,  8, tmconj     = allocVector(INTSXP, nconds * estim1));
    SET_VECTOR_ELT(usage,  9, tmdisj     = allocVector(INTSXP, nconds * estim2));
    SET_VECTOR_ELT(usage, 10, ticpr_conj = allocVector(REALSXP, 3 * estim1));    
    SET_VECTOR_ELT(usage, 11, ticpr_disj = allocVector(REALSXP, 3 * estim2));    
    SET_VECTOR_ELT(usage, 12, tcoms_conj = allocMatrix(REALSXP, xrows, estim1)); 
    SET_VECTOR_ELT(usage, 13, tcoms_disj = allocMatrix(REALSXP, xrows, estim1)); 
    SET_VECTOR_ELT(usage, 14, indx_conj  = allocVector(INTSXP, p_depth[0] * estim1)); 
    SET_VECTOR_ELT(usage, 15, indx_disj  = allocVector(INTSXP, p_depth[0] * estim2)); 
    SET_VECTOR_ELT(usage, 16, ck_conj    = allocVector(INTSXP, estim1)); 
    SET_VECTOR_ELT(usage, 17, ck_disj    = allocVector(INTSXP, estim2)); 
    int    *p_tmconj     = INTEGER(tmconj);
    int    *p_tmdisj     = INTEGER(tmdisj);
    double *p_ticpr_conj = REAL(ticpr_conj);
    double *p_ticpr_disj = REAL(ticpr_disj);
    double *p_tcoms_conj = REAL(tcoms_conj);
    double *p_tcoms_disj = REAL(tcoms_disj);
    int    *p_indx_conj  = INTEGER(indx_conj);
    int    *p_indx_disj  = INTEGER(indx_disj);
    int    *p_ck_conj    = INTEGER(ck_conj);
    int    *p_ck_disj    = INTEGER(ck_disj);
    double copyline[nconds], minx[xrows], maxx[xrows];
    double incovpron[6];
    double so = 0.0,
           min, max,
           sum_minx,
           sum_maxx,
           sum_1_minx,
           sum_1_maxx,
           sum_1_min_y_minx,
           sum_1_min_y_maxx,
           sum_min_y_minx,
           sum_min_y_maxx,
           prisum_minx,
           prisum_maxx,
           tmpv11, tmpv12, tmpv21, tmpv22;
    int found1 = 0;
    int found = 0;
    int foundk1 = 0;
    int foundk2 = 0;
    if (nconds < p_depth[0]) {
        p_depth[0] = nconds;
    }
    for (int i = 0; i < length(vo); i++) {
        so += p_vo[i];
    }
    int chkred[nconds], inclcov;
    int k = 1;
    int foundk = 1;
    while (k <= p_depth[0] && foundk) {
        if (found1 + found > 0 && k > 3) {
            foundk = 0;
        }
        int klnofl[k];
        int tempk[k];
        for (int i = 0; i < k; i++) {
            tempk[i] = i;
        }
        int e = 0;
        int h = k;
        int kmatcol = 0;
        while (tempk[0] != nconds - k || !kmatcol) {
            if (kmatcol) {
                increment(k, &e, &h, nconds, tempk, 0);
            }
            kmatcol = 1;
            int klcols[k];
            int klrows = 1;
            for (int j = 0; j < k; j++) {
                klnofl[j] = p_noflevels[tempk[j]];
                klrows *= klnofl[j];
                klcols[j] = j;
            }
            SET_VECTOR_ELT(usage, 18, combkl = allocVector(INTSXP, klrows * k));
            int *p_combkl = INTEGER(combkl);
            fillMatrix(klrows, k, klnofl, p_combkl, 0, klcols, 0);
            for (int kli = 0; kli < klrows; kli++) {
                for (int c = 0; c < nconds; c++) {
                    chkred[c] = 0;
                }
                for (int j = 0; j < k; j++) {
                    chkred[tempk[j]] = p_combkl[j * klrows + kli] + 1; 
                }
                sum_minx = 0;         
                sum_maxx = 0;         
                sum_min_y_minx = 0;   
                sum_min_y_maxx = 0;   
                prisum_minx = 0;      
                prisum_maxx = 0;      
                sum_1_minx = 0;       
                sum_1_min_y_minx = 0; 
                sum_1_maxx = 0;       
                sum_1_min_y_maxx = 0; 
                for (int r = 0; r < xrows; r++) { 
                    min = 1000000;        
                    max = 0;
                    for (int c = 0; c < xcols; c++) { 
                        copyline[c] = p_x[c * xrows + r];
                        if (p_fuz[c]) { 
                            if (chkred[c] == 1) {
                                copyline[c] = 1 - copyline[c];
                            }
                        }
                        else {
                            if (chkred[c] == (copyline[c] + 1)) {
                                copyline[c] = 1; 
                            }
                            else {
                                copyline[c] = 0; 
                            }
                        }
                        if (chkred[c] != 0) {
                            if (copyline[c] < min) {
                                min = copyline[c]; 
                            }
                            if (copyline[c] > max) {
                                max = copyline[c]; 
                            }
                        }
                    } 
                    minx[r] = min;   
                    maxx[r] = max;   
                    sum_minx += min; 
                    sum_maxx += max; 
                    sum_min_y_minx += (min < p_vo[r])?min:p_vo[r];
                    sum_min_y_maxx += (max < p_vo[r])?max:p_vo[r];
                    if (p_nec[0]) {  
                        sum_1_minx += 1 - min;                                  
                        sum_1_maxx += 1 - max;                                  
                        sum_1_min_y_minx += 1 - ((min < p_vo[r])?min:p_vo[r]);  
                        sum_1_min_y_maxx += 1 - ((max < p_vo[r])?max:p_vo[r]);  
                    }
                    else {           
                        tmpv11 = (min < p_vo[r])?min:p_vo[r];
                        tmpv12 = p_nec[0]?(1 - min):(1 - p_vo[r]);
                        prisum_minx += (tmpv11 < tmpv12)?tmpv11:tmpv12;
                        tmpv21 = (max < p_vo[r])?max:p_vo[r];
                        tmpv22 = 1 - max;
                        prisum_maxx += (tmpv21 < tmpv22)?tmpv21:tmpv22;
                    }
                } 
                incovpron[0] = (sum_min_y_minx == 0 && sum_minx == 0)?0:(sum_min_y_minx/sum_minx);
                incovpron[1] = (sum_min_y_minx == 0 && so == 0)?0:(sum_min_y_minx/so);
                incovpron[2] = (sum_min_y_maxx == 0 && so == 0)?0:(sum_min_y_maxx/so);
                incovpron[3] = (sum_min_y_maxx == 0 && sum_maxx == 0)?0:(sum_min_y_maxx/sum_maxx);
                if (p_nec[0]) {
                    incovpron[4] = (sum_1_minx == 0 && sum_1_min_y_minx == 0)?0:(sum_1_minx/sum_1_min_y_minx);
                    incovpron[5] = (sum_1_maxx == 0 && sum_1_min_y_maxx == 0)?0:(sum_1_maxx/sum_1_min_y_maxx);
                }
                else {
                    tmpv11 = sum_min_y_minx - prisum_minx;
                    tmpv12 = (p_nec[0]?so:sum_minx) - prisum_minx;
                    incovpron[4] = (tmpv11 == 0 && tmpv12 == 0)?0:(tmpv11/tmpv12);
                    tmpv21 = sum_min_y_maxx - prisum_maxx;
                    tmpv22 = so - prisum_maxx;
                    incovpron[5] = (tmpv21 == 0 && tmpv22 == 0)?0:(tmpv21/tmpv22);
                }
                inclcov = incovpron[p_nec[0]] >= p_inclcut[0] && incovpron[1 - p_nec[0]] >= p_covcut[0];
                int redundant = 0;
                if (inclcov) {
                    if (foundk1 > 0 && !p_nec[0]) { 
                        int i = 0;
                        while (i < foundk1 && !redundant) {
                            int sumeq = 0;
                            int v = 0;
                            while (sumeq == v && v < p_ck_conj[i]) {
                                for (int c = 0; c < k; c++) {
                                    if (p_indx_conj[i * p_depth[0] + v] == tempk[c] + 1) {
                                        sumeq += (p_tmconj[i * nconds + p_indx_conj[i * p_depth[0] + v] - 1] == chkred[tempk[c]]);
                                    }
                                }
                                v += 1;
                            }
                            if (sumeq == v) {
                                redundant = 1;
                            }
                            i += 1;
                        }
                    }
                    if (!redundant) { 
                        for (int c = 0; c < nconds; c++) {
                            p_tmconj[found1 * nconds + c] = chkred[c];
                        }
                        p_ticpr_conj[found1 * 3 + 0] = incovpron[p_nec[0]];
                        p_ticpr_conj[found1 * 3 + 1] = incovpron[4];
                        p_ticpr_conj[found1 * 3 + 2] = incovpron[1 - p_nec[0]];
                        for (int r = 0; r < xrows; r++) {
                            p_tcoms_conj[found1 * xrows + r] = minx[r];
                        }
                        for (int c = 0; c < k; c++) {
                            p_indx_conj[p_depth[0] * found1 + c] = tempk[c] + 1;
                        }
                        p_ck_conj[found1] = k;
                        foundk += 1;
                        found1 += 1;
                        if (found1 == estim1) {
                            int copytm_conj[nconds * found1];
                            int tindx_conj[p_depth[0] * found1];
                            int tck_conj[found1];
                            double copyticpr_conj[3 * found1];
                            double copytcoms_conj[xrows * found1];
                            for (int i = 0; i < nconds * found1; i++) {
                                copytm_conj[i] = p_tmconj[i];
                            }
                            for (int i = 0; i < 3 * found1; i++) {
                                copyticpr_conj[i] = p_ticpr_conj[i];
                            }
                            for (int i = 0; i < xrows * found1; i++) {
                                copytcoms_conj[i] = p_tcoms_conj[i];
                            }
                            for (int i = 0; i < p_depth[0] * found1; i++) {
                                tindx_conj[i] = p_indx_conj[i];
                            }
                            for (int i = 0; i < found1; i++) {
                                tck_conj[i] = p_ck_conj[i];
                            }
                            estim1 *= 2;
                            SET_VECTOR_ELT(usage, 8,  tmconj     = allocVector(INTSXP, nconds * estim1));
                            p_tmconj = INTEGER(tmconj);
                            SET_VECTOR_ELT(usage, 10, ticpr_conj = allocVector(REALSXP, 3 * estim1));
                            p_ticpr_conj = REAL(ticpr_conj);
                            SET_VECTOR_ELT(usage, 12, tcoms_conj = allocMatrix(REALSXP, xrows, estim1));
                            p_tcoms_conj = REAL(tcoms_conj);
                            SET_VECTOR_ELT(usage, 14, indx_conj  = allocVector(INTSXP, p_depth[0] * estim1));
                            p_indx_conj = INTEGER(indx_conj);
                            SET_VECTOR_ELT(usage, 16, ck_conj    = allocVector(INTSXP, estim1));
                            p_ck_conj = INTEGER(ck_conj);
                            for (int i = 0; i < nconds * found1; i++) {
                                p_tmconj[i] = copytm_conj[i];
                            }
                            for (int i = 0; i < 3 * found1; i++) {
                                p_ticpr_conj[i] = copyticpr_conj[i];
                            }
                            for (int i = 0; i < xrows * found1; i++) {
                                p_tcoms_conj[i] = copytcoms_conj[i];
                            }
                            for (int i = 0; i < p_depth[0] * found1; i++) {
                                p_indx_conj[i] = tindx_conj[i];
                            }
                            for (int i = 0; i < found1; i++) {
                                p_ck_conj[i] = tck_conj[i];
                            }
                        }
                    }
                }
                else {
                    if (p_nec[0]) {
                        inclcov = incovpron[2] >= p_inclcut[0] && incovpron[3] >= p_covcut[0];
                        redundant = 0;
                        if (inclcov && foundk1 > 0) {
                            int i = 0;
                            while (i < foundk1 && !redundant) {
                                int sumeq = 0;
                                int v = 0;
                                while (sumeq == v && v < p_ck_conj[i]) {
                                    for (int c = 0; c < k; c++) {
                                        if (p_indx_conj[i * p_depth[0] + v] == tempk[c] + 1) {
                                            sumeq += (p_tmconj[i * nconds + p_indx_conj[i * p_depth[0] + v] - 1] == chkred[tempk[c]]);
                                        }
                                    }
                                    v += 1;
                                }
                                if (sumeq == v) {
                                    redundant = 1;
                                }
                                i += 1;
                            }
                        }
                        if (inclcov && foundk2 > 0 && !redundant) {
                            int i = 0;
                            while (i < foundk2 && !redundant) {
                                int sumeq = 0;
                                int v = 0;
                                while (sumeq == v && v < p_ck_disj[i]) {
                                    for (int c = 0; c < k; c++) {
                                        if (p_indx_disj[i * p_depth[0] + v] == tempk[c] + 1) {
                                            sumeq += (p_tmdisj[i * nconds + p_indx_disj[i * p_depth[0] + v] - 1] == chkred[tempk[c]]);
                                        }
                                    }
                                    v += 1;
                                }
                                if (sumeq == v) {
                                    redundant = 1;
                                }
                                i += 1;
                            }
                        }
                        if (inclcov && !redundant) {
                            for (int c = 0; c < nconds; c++) {
                                p_tmdisj[found * nconds + c] = chkred[c];
                            }
                            p_ticpr_disj[found * 3 + 0] = incovpron[2];
                            p_ticpr_disj[found * 3 + 1] = incovpron[5];
                            p_ticpr_disj[found * 3 + 2] = incovpron[3];
                            for (int r = 0; r < xrows; r++) {
                                p_tcoms_disj[found * xrows + r] = maxx[r];
                            }
                            p_ck_disj[found] = k;
                            for (int c = 0; c < k; c++) {
                                p_indx_disj[p_depth[0] * found + c] = tempk[c] + 1;
                            }
                            foundk += 1;
                            found += 1;
                            if (found == estim2) {
                                int copytmdisj[nconds * found];
                                int tindx_disj[p_depth[0] * found];
                                int tck_disj[found];
                                double copyticpr_disj[3 * found];
                                double copytcoms_disj[xrows * found];
                                for (int i = 0; i < nconds * found; i++) {
                                    copytmdisj[i] = p_tmdisj[i];
                                }
                                for (int i = 0; i < 3 * found; i++) {
                                    copyticpr_disj[i] = p_ticpr_disj[i];
                                }
                                for (int i = 0; i < xrows * found; i++) {
                                    copytcoms_disj[i] = p_tcoms_disj[i];
                                }
                                for (int i = 0; i < p_depth[0] * found; i++) {
                                    tindx_disj[i] = p_indx_disj[i];
                                }
                                for (int i = 0; i < found; i++) {
                                    tck_disj[i] = p_ck_disj[i];
                                }
                                estim2 *= 2;
                                SET_VECTOR_ELT(usage,  9, tmdisj     = allocVector(INTSXP, nconds * estim2));
                                p_tmdisj = INTEGER(tmdisj);
                                SET_VECTOR_ELT(usage, 11, ticpr_disj = allocVector(REALSXP, 3 * estim2));
                                p_ticpr_disj = REAL(ticpr_disj);
                                SET_VECTOR_ELT(usage, 13, tcoms_disj = allocMatrix(REALSXP, xrows, estim2));
                                p_tcoms_disj = REAL(tcoms_disj);
                                SET_VECTOR_ELT(usage, 15, indx_disj  = allocVector(REALSXP, p_depth[0] * estim2));
                                p_indx_disj = INTEGER(indx_disj);
                                SET_VECTOR_ELT(usage, 17, ck_disj    = allocVector(INTSXP, estim2));
                                p_ck_disj = INTEGER(ck_disj);
                                for (int i = 0; i < nconds * found; i++) {
                                    p_tmdisj[i] = copytmdisj[i];
                                }
                                for (int i = 0; i < 3 * found; i++) {
                                    p_ticpr_disj[i] = copyticpr_disj[i];
                                }
                                for (int i = 0; i < xrows * found; i++) {
                                    p_tcoms_disj[i] = copytcoms_disj[i];
                                }
                                for (int i = 0; i < p_depth[0] * found; i++) {
                                    p_indx_disj[i] = tindx_disj[i];
                                }
                                for (int i = 0; i < found; i++) {
                                    p_ck_disj[i] = tck_disj[i];
                                }
                            }
                        }
                    }
                } 
            } 
        }
        foundk1 = found1;
        foundk2 = found;
        k++;
    }
    SEXP icpr_conj, icpr_disj, mconj, mdisj, coms_conj, coms_disj;
    SEXP result = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(result, 0, icpr_conj = allocMatrix(REALSXP, found1, 3)); 
    SET_VECTOR_ELT(result, 1, icpr_disj = allocMatrix(REALSXP, found, 3)); 
    SET_VECTOR_ELT(result, 2, mconj = allocMatrix(INTSXP, found1, nconds)); 
    SET_VECTOR_ELT(result, 3, mdisj = allocMatrix(INTSXP, found, nconds)); 
    SET_VECTOR_ELT(result, 4, coms_conj = allocMatrix(REALSXP, xrows, found1)); 
    SET_VECTOR_ELT(result, 5, coms_disj = allocMatrix(REALSXP, xrows, found)); 
    double *p_icpr_conj = REAL(icpr_conj);
    double *p_icpr_disj = REAL(icpr_disj);
    int *p_mconj = INTEGER(mconj);
    int *p_mdisj = INTEGER(mdisj);
    double *p_coms_conj = REAL(coms_conj);
    double *p_coms_disj = REAL(coms_disj);
    for (int r = 0; r < found1; r++) { 
        for (int c = 0; c < 3; c++) {
            p_icpr_conj[c * found1 + r] = p_ticpr_conj[r * 3 + c];
        }
    }
    for (int r = 0; r < found1; r++) { 
        for (int c = 0; c < nconds; c++) {
            p_mconj[c * found1 + r] = p_tmconj[r * nconds + c];
        }
    }
    for (int r = 0; r < found; r++) { 
        for (int c = 0; c < 3; c++) {
            p_icpr_disj[c * found + r] = p_ticpr_disj[r * 3 + c];
        }
    }
    for (int r = 0; r < found; r++) { 
        for (int c = 0; c < nconds; c++) {
            p_mdisj[c * found + r] = p_tmdisj[r * nconds + c];
        }
    }
    for (int i = 0; i < xrows * found1; i++) {
        p_coms_conj[i] = p_tcoms_conj[i];
    }
    for (int i = 0; i < xrows * found; i++) {
        p_coms_disj[i] = p_tcoms_disj[i];
    }
    UNPROTECT(2);
    return(result);
}
SEXP C_QMC(SEXP tt, SEXP noflevels) {
    SEXP pimat, tempmat, minimized, copymat, order, cl; 
    int *p_tt, *p_noflevels, *p_pimat, *p_tempmat, *p_minimized, *p_copymat, *p_order,  *p_cl;
    SEXP usage = PROTECT(allocVector(VECSXP, 10));
    SEXP dimnames, colnms;
    SET_VECTOR_ELT(usage, 8, dimnames = getAttrib(tt, R_DimNamesSymbol));
    SET_VECTOR_ELT(usage, 9, colnms = getAttrib(tt, R_DimNamesSymbol));
    if (!Rf_isNull(dimnames)) {
        colnms = VECTOR_ELT(getAttrib(tt, R_DimNamesSymbol), 1);
    }
    SET_VECTOR_ELT(usage, 0, tt = coerceVector(tt, INTSXP));
    p_tt = INTEGER(tt);
    SET_VECTOR_ELT(usage, 1, noflevels = coerceVector(noflevels, INTSXP));
    p_noflevels = INTEGER(noflevels);
    int nimplicants = nrows(tt); 
    int nconds = ncols(tt);
    SET_VECTOR_ELT(usage, 2, tempmat = allocMatrix(INTSXP, nconds, nimplicants));
    p_tempmat = INTEGER(tempmat);
    for (int r = 0; r < nimplicants; r++) {
        for (int c = 0; c < nconds; c++) {
            p_tempmat[r * nconds + c] = p_tt[c * nimplicants + r];  
        }
    }
    int found = 1;
    int temp[nconds];
    int combs[2];
    int e, h;
    int iteration = 0;
    while (found > 0 && nimplicants > 1) {
        iteration++;
        found = 0;
        SET_VECTOR_ELT(usage, 3, minimized = allocVector(LGLSXP, nimplicants));
        p_minimized = LOGICAL(minimized);
        memset(p_minimized, FALSE, nimplicants * sizeof(int));
        int estimpi = 10000;
        SET_VECTOR_ELT(usage, 4, pimat = allocMatrix(INTSXP, nconds, estimpi));
        p_pimat = INTEGER(pimat);
        combs[0] = 0;
        combs[1] = 0;
        e = 0;
        h = 2; 
        Rboolean last = (nimplicants == 2);
        while (combs[0] != nimplicants - 2 || last) { 
            if (nimplicants == 2) {
                combs[1] = 1;
            }
            else {
                increment(2, &e, &h, nimplicants, combs, 0);
            }
            last = FALSE;
            int r = 0;
            int diffs = 0;
            int which = 0;
            Rboolean comparable = TRUE;
            while (diffs < 2 && r < nconds && comparable) {
                temp[r] = p_tempmat[combs[0] * nconds + r];
                if (temp[r] != p_tempmat[combs[1] * nconds + r]) {
                    comparable = temp[r] > 0 && p_tempmat[combs[1] * nconds + r] > 0;
                    diffs++;
                    which = r;
                    temp[r] = 0;
                }
                r++;
            }
            if (diffs == 1 && comparable) {
                int minrows[p_noflevels[which]];
                minrows[0] = combs[0];
                minrows[1] = combs[1];
                int tominimize = 2;
                int c = 0;
                while (c < nimplicants && tominimize < p_noflevels[which]) {
                    if (c != combs[0] && c != combs[1]) {
                        Rboolean equal = TRUE;
                        int r = 0;
                        while (r < nconds && equal) {
                            if (r != which) {
                                equal = temp[r] == p_tempmat[c * nconds + r];
                            }
                            r++;
                        }
                        if (equal) {
                            minrows[tominimize] = c;
                            tominimize++;
                        }
                    }
                    c++;
                }
                if (tominimize == p_noflevels[which]) {
                    for (int i = 0; i < tominimize; i++) {
                        p_minimized[minrows[i]] = TRUE;
                    }
                    int f = 0;
                    Rboolean unique = TRUE;
                    while (f < found && unique) {
                        Rboolean equal = TRUE;
                        int r = 0;
                        while (r < nconds && equal) {
                            equal = temp[r] == p_pimat[f * nconds + r];
                            r++;
                        }
                        unique = !equal;
                        f++;
                    }
                    if (unique) {
                        for (int r = 0; r < nconds; r++) {
                            p_pimat[found * nconds + r] = temp[r];
                        }
                        found++;
                    }
                }
            }
            if (found == estimpi) {
                estimpi *= 2;
                int totlent = found * nconds;
                SET_VECTOR_ELT(usage, 5, copymat = allocVector(INTSXP, totlent));
                p_copymat = INTEGER(copymat);
                for (int i = 0; i < totlent; i++) {
                    p_copymat[i] = p_pimat[i];
                }
                SET_VECTOR_ELT(usage, 4, pimat = allocMatrix(INTSXP, nconds, estimpi));
                p_pimat = INTEGER(pimat);
                for (int i = 0; i < totlent; i++) {
                    p_pimat[i] = p_copymat[i];
                }
            }
        }
        int nonmin = 0;
        for (int i = 0; i < nimplicants; i++) {
            nonmin += !p_minimized[i];
        }
        SET_VECTOR_ELT(usage, 5, copymat = allocVector(INTSXP, (found + nonmin) * nconds));
        p_copymat = INTEGER(copymat);
        int foundlent = found * nconds;
        for (int i = 0; i < foundlent; i++) {
            p_copymat[i] = p_pimat[i];
        }
        if (nonmin > 0) {
            for (int i = 0; i < nimplicants; i++) {
                if (!p_minimized[i]) {
                    for (int r = 0; r < nconds; r++) {
                        p_copymat[foundlent + r] = p_tempmat[i * nconds + r];
                    }
                    foundlent += nconds;
                }
            }
        }
        SET_VECTOR_ELT(usage, 2, tempmat = allocMatrix(INTSXP, nconds, found + nonmin));
        p_tempmat = INTEGER(tempmat);
        for (int i = 0; i < foundlent; i++) {
            p_tempmat[i] = p_copymat[i];
        }
        nimplicants = ncols(tempmat);
    }
    SET_VECTOR_ELT(usage, 6, order = allocVector(INTSXP, nimplicants));
    p_order = INTEGER(order);
    SET_VECTOR_ELT(usage, 7, cl = allocVector(INTSXP, nimplicants));
    p_cl = INTEGER(cl);
    for (int c = 0; c < nimplicants; c++) {
        p_cl[c] = nconds;
        for (int r = 0; r < nconds; r++) {
            p_cl[c] -= p_tempmat[c * nconds + r] == 0 ? 1 : 0;
        }
    }
    sortmat(p_tempmat, p_order, p_cl, nconds, nimplicants);
    SET_VECTOR_ELT(usage, 5, copymat = allocMatrix(INTSXP, nimplicants, nconds));
    p_copymat = INTEGER(copymat);
    for (int c = 0; c < nconds; c++) {
        for (int r = 0; r < nimplicants; r++) {
            p_copymat[c * nimplicants + r] = p_tempmat[p_order[r] * nconds + c]; 
        }
    }
    if (!Rf_isNull(dimnames)) {
        SET_VECTOR_ELT(usage, 8, dimnames = allocVector(VECSXP, 2));
        SET_VECTOR_ELT(dimnames, 1, colnms);
        setAttrib(copymat, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(1);
    return(copymat);
}
SEXP C_removeRedundants(SEXP rowno, SEXP noflevels, SEXP mbase) {
    int *pointer_next, *pointer_final, *pointer_temp1, *pointer_temp2, *pointer_rowno, *pointer_noflevels, *pointer_mbase;
    int previous, lmbase, ltemp2, lrowno, lmbasei, i, j, k, rn, finalength, lungime, flag2, flag1, templung;
    SEXP next, final, temp1, temp2;
    SEXP usage = PROTECT(allocVector(VECSXP, 7));
    SET_VECTOR_ELT(usage, 0, rowno = coerceVector(rowno, INTSXP));
    SET_VECTOR_ELT(usage, 1, noflevels = coerceVector(noflevels, INTSXP));
    SET_VECTOR_ELT(usage, 2, mbase = coerceVector(mbase, INTSXP));
    pointer_rowno = INTEGER(rowno);
    pointer_noflevels = INTEGER(noflevels);
    pointer_mbase = INTEGER(mbase);
    lmbase = length(mbase);
    lrowno = length(rowno);
	SET_VECTOR_ELT(usage, 3, next = allocVector(INTSXP, lrowno));
	pointer_next = INTEGER(next);
	
	for (i = 0; i < lrowno; i++) {
	    pointer_next[i] = i + 1;
	}
	
	rn = 0;
	flag1 = 0;
	
	while (rn < lrowno) {
	    templung = 1;
	    previous = rn;
        SET_VECTOR_ELT(usage, 4, temp1 = allocVector(INTSXP, 1));
        pointer_temp1 = INTEGER(temp1);
        pointer_temp1[0] = pointer_rowno[rn];
        flag2 = 0;
        for (i = 0; i < lmbase; i++) {
            lmbasei = lmbase - i - 1;
            if (div(div(pointer_rowno[rn] - 1, pointer_mbase[lmbasei]).quot, pointer_noflevels[lmbasei] + 1).rem == 0) {
                flag2 = 1;
                lungime = templung * (pointer_noflevels[lmbasei] + 1);
                SET_VECTOR_ELT(usage, 5, temp2 = allocVector(INTSXP, lungime));
                pointer_temp2 = INTEGER(temp2);
                for (j = 0; j < length(temp1); j++) {
                    pointer_temp2[j] = pointer_temp1[j];
                    for (k = 0; k < pointer_noflevels[lmbasei]; k++) {
                        pointer_temp2[j + length(temp1)*(k + 1)] = pointer_temp1[j] + (k + 1)*pointer_mbase[lmbasei];
                    }
                }
                if (i < lmbase) {
                    SET_VECTOR_ELT(usage, 4, temp1 = allocVector(INTSXP, lungime));
                    pointer_temp1 = INTEGER(temp1);
                    for (j = 0; j < lungime; j++) {
                        pointer_temp1[j] = pointer_temp2[j];
                    }
                    templung = lungime;
                }
            }
        }
        if (flag2 == 1) { 
            ltemp2 = length(temp2);
            i = pointer_next[previous];
            j = 0;
            while (i < lrowno && j < ltemp2) {
                if (pointer_rowno[i] < pointer_temp2[j]) {
                    previous = i;
                    i = pointer_next[i];
                }
                else if (pointer_rowno[i] > pointer_temp2[j]) {
                    j++;
                }
                else { 
                    flag1 = 1;
                    pointer_next[previous] = pointer_next[i];
                    i = pointer_next[i];
                    j++;
                }
            }
        }
		rn = pointer_next[rn];
    }
    if (flag1 == 0) { 
        UNPROTECT(1);
        return(rowno);
    }
    else {
        finalength = 0;
        i = 0;
        while (i < lrowno) {
            i = pointer_next[i];
            finalength++;
        }
        SET_VECTOR_ELT(usage, 6, final = allocVector(INTSXP, finalength));
        pointer_final = INTEGER(final);
        i = 0;
        j = 0;
        while (i < lrowno) {
            pointer_final[j] = pointer_rowno[i];
            i = pointer_next[i];
            j += 1; 
        }
        UNPROTECT(1);
        return(final);
    }
}
SEXP C_combinations(SEXP list) {
    int nconds, k, aloe, zero;
    nconds = INTEGER(VECTOR_ELT(list, 0))[0];
    k = INTEGER(VECTOR_ELT(list, 1))[0];
    aloe = INTEGER(VECTOR_ELT(list, 2))[0] - 1;
    zero = INTEGER(VECTOR_ELT(list, 3))[0];
    int nck = 1;
    for (int i = 1; i <= k; i++) {
        nck *= nconds - (k - i);
        nck /=  i;
    }
    SEXP usage = PROTECT(allocVector(VECSXP, 2));
    SEXP out;
    SET_VECTOR_ELT(usage, 0, out = allocMatrix(INTSXP, k, nck));
    int *p_out = INTEGER(out);
    int tempk[k];
    for (int i = 0; i < k; i++) {
        tempk[i] = i;
    }
    tempk[k - 1] -= 1;
    int e = 0;
    int h = k;
    Rboolean last = (nconds == k);
    int found = 0;
    while ((tempk[0] != nconds - k) || last) {
        increment(k, &e, &h, nconds + last, tempk, aloe);
        last = FALSE;
        for (int i = 0; i < k; i++) {
            p_out[found * k + i] = tempk[i] + zero;
        }
        found += 1;
    }
    if (found < nck) {
        SEXP copy;
        SET_VECTOR_ELT(usage, 1, copy = duplicate(out));
        int *p_copy = INTEGER(copy);
        SET_VECTOR_ELT(usage, 0, out = allocMatrix(INTSXP, k, found));
        p_out = INTEGER(out);
        for (int i = 0; i < found * k; i++) {
            p_out[i] = p_copy[i];
        }
    }
    UNPROTECT(1);
    return(out);
}
SEXP C_findSubsets(SEXP rowno, SEXP noflevels, SEXP mbase, SEXP max) {
    int *prowno, *pnoflevels, *pmbase, *pmax, lmbase, lmbasei, i, j, k, lungime, flag, templung, *ptemp1, *ptemp2;
    SEXP temp1, temp2;
    SEXP usage = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(usage, 0, rowno = coerceVector(rowno, INTSXP));
    SET_VECTOR_ELT(usage, 1, noflevels = coerceVector(noflevels, INTSXP));
    SET_VECTOR_ELT(usage, 2, mbase = coerceVector(mbase, INTSXP));
    prowno = INTEGER(rowno);
    pnoflevels = INTEGER(noflevels);
    pmbase = INTEGER(mbase);
    if (max == R_NilValue) {
        SET_VECTOR_ELT(usage, 3, max = allocVector(INTSXP, 1));
        pmax = INTEGER(max);
        pmax[0] = prowno[length(rowno) - 1];
    }
    else {
        SET_VECTOR_ELT(usage, 3, max = coerceVector(max, INTSXP));
        pmax = INTEGER(max);
    }
    SET_VECTOR_ELT(usage, 4, temp1 = allocVector(INTSXP, 1));
    ptemp1 = INTEGER(temp1);
    ptemp1[0] = prowno[0];
    flag = 0;
    lmbase = length(mbase);
    templung = 1;
    for (i = 0; i < lmbase; i++) {
        lmbasei = lmbase - i - 1;
        if (div(div(prowno[0] - 1, pmbase[lmbasei]).quot, pnoflevels[lmbasei] + 1).rem == 0) {
            flag = 1;
            lungime = templung * (pnoflevels[lmbasei] + 1);
            SET_VECTOR_ELT(usage, 5, temp2 = allocVector(INTSXP, lungime));
            ptemp2 = INTEGER(temp2);
            for (j = 0; j < length(temp1); j++) {
                ptemp2[j] = ptemp1[j];
                for (k = 0; k < pnoflevels[lmbasei]; k++) {
                    ptemp2[j + length(temp1)*(k + 1)] = ptemp1[j] + (k + 1)*pmbase[lmbasei];
                }
            }
            if (i < length(mbase)) {
                SET_VECTOR_ELT(usage, 4, temp1 = allocVector(INTSXP, lungime));
                ptemp1 = INTEGER(temp1);
                for (j = 0; j < lungime; j++) {
                    ptemp1[j] = ptemp2[j];
                }
                templung = lungime;
            }
        }
    }
    if (flag == 1) {
        templung = 0;
        for (i = 0; i < lungime; i++) {
            if (ptemp2[i] < (pmax[0] + 1)) {
                templung += 1;
            }
        }
        SET_VECTOR_ELT(usage, 4, temp1 = allocVector(INTSXP, templung - 1)); 
        ptemp1 = INTEGER(temp1);
        j = 0;
        for (i = 1; i < lungime; i++) {
            if (ptemp2[i] < pmax[0] + 1) {
                ptemp1[j] = ptemp2[i];
                j += 1;
            }
        }
    }
    else {
        UNPROTECT(1);
        return(R_NilValue);
    }
    UNPROTECT(1);
    return(temp1);
}
SEXP C_getEC(SEXP aleabune, SEXP veverita, SEXP catelus, SEXP ursulet, SEXP ratusca, SEXP ametist) {
    SEXP usage = PROTECT(allocVector(VECSXP, 6));
    int *p_aleabune   = INTEGER(aleabune);
    int *p_veverita = INTEGER(veverita);
    int *p_catelus = INTEGER(catelus);
    int *p_ursulet = INTEGER(ursulet);
    int *p_ratusca = INTEGER(ratusca);
    int nc_aleabune   = ncols(aleabune);
    int nr_aleabune   = nrows(aleabune);
    int nr_veverita = nrows(veverita);
    int nc_catelus = ncols(catelus);
    int nr_catelus = nrows(catelus);
    int nr_ursulet = nrows(ursulet);
    int nc_ratusca = ncols(ratusca);
    int nr_ratusca = nrows(ratusca);
    int magarii[nr_aleabune];
    int cronicar = 0;
    int alambic = 0;
    for (int r = 0; r < nr_aleabune; r++) {
        magarii[r] = 0;
        for (int c = 0; c < nc_aleabune; c++) {
            if (p_aleabune[c * nr_aleabune + r] > 0) {
                if (magarii[r] == 0) {
                    cronicar += 1;
                }
                else {
                    cronicar -= 1;
                    alambic += 1;
                }
                magarii[r] += 1;
            }
        }
    }
    SEXP carare, poteca;
    SET_VECTOR_ELT(usage, 1, carare = allocVector(INTSXP, cronicar));
    int *p_carare = INTEGER(carare);
    SET_VECTOR_ELT(usage, 2, poteca = allocVector(INTSXP, alambic));
    int *p_poteca = INTEGER(poteca);
    int scris = 0, oral = 0;
    for (int r = 0; r < nr_aleabune; r++) {
        if (magarii[r] == 1) {
            p_carare[scris] = r;
            scris++;
        }
        else {
            p_poteca[oral] = r;
            oral++;
        }
    }
    SEXP EClist = PROTECT(allocVector(VECSXP, nc_catelus * nc_ratusca));
    int calare[nc_aleabune];
    int pejos[nc_aleabune];
    int balarie[nc_aleabune];
    SEXP alearele; 
    int *p_alearele;
    int mirare = 0;
    for (int rotund = 0; rotund < nc_catelus; rotund++) { 
        int calaret = 0;
        for (int r = 0; r < nr_catelus; r++) {
            if (p_catelus[rotund * nr_catelus + r] > 0) {
                calaret++;
            }
        }
        for (int oval = 0; oval < nc_ratusca; oval++) { 
            int catranit = 0;
            for (int r = 0; r < nr_ratusca; r++) {
                if (p_ratusca[oval * nr_ratusca + r] > 0) {
                    catranit++;
                }
            }
            SET_VECTOR_ELT(usage, 3, alearele = VECTOR_ELT(ametist, oval));
            p_alearele = INTEGER(alearele);
            int nr_alearele = nrows(alearele);
            Rboolean palarie[nr_alearele];
            for (int r = 0; r < nr_alearele; r++) {
                palarie[r] = FALSE;
            }
            for (int croseu = 0; croseu < calaret; croseu++) {
                for (int c = 0; c < nc_aleabune; c++) {
                    calare[c] = p_veverita[c * nr_veverita + p_catelus[rotund * nr_catelus + croseu] - 1];
                }
                for (int upercut = 0; upercut < catranit; upercut++) {
                    Rboolean dinfata = TRUE;
                    Rboolean rosiatic[nc_aleabune]; 
                    Rboolean cenusiu[nc_aleabune]; 
                    int c = 0;
                    while (dinfata && c < nc_aleabune) {
                        pejos[c] = p_ursulet[c * nr_ursulet + p_ratusca[oval * nr_ratusca + upercut] - 1];
                        rosiatic[c] = pejos[c] > 0;
                        cenusiu[c] = calare[c] > 0 && !rosiatic[c]; 
                        if (rosiatic[c]) {
                            dinfata = pejos[c] == calare[c];
                        }
                        c++;
                    }
                    if (dinfata) {
                        if (cronicar > 0) { 
                            for (int c = 0; c < nc_aleabune; c++) {
                                balarie[c] = pejos[c];
                                if (cenusiu[c]) {
                                    Rboolean banana = TRUE;
                                    for (int r = 0; r < cronicar; r++) {
                                        int galetusa = p_aleabune[c * nr_aleabune + p_carare[r]];
                                        if (galetusa > 0) {
                                            banana = FALSE;
                                            if (galetusa == calare[c]) {
                                                balarie[c] = galetusa;
                                            }
                                        }
                                    }
                                    if (banana) {
                                        balarie[c] = calare[c];
                                    }
                                }
                            }
                            for (int r = 0; r < nr_alearele; r++) {
                                if (!palarie[r]) {
                                    Rboolean nerod = TRUE;
                                    int c = 0;
                                    while (nerod && c < nc_aleabune) {
                                        nerod = (balarie[c] > 0) ? p_alearele[c * nr_alearele + r] + 1 == balarie[c] : TRUE;
                                        c++;
                                    }
                                    palarie[r] = nerod;
                                }
                            }
                        }
                        if (alambic > 0) { 
                            for (int r = 0; r < alambic; r++) {
                                Rboolean toatecele = TRUE;
                                int c = 0;
                                while (toatecele && c < nc_aleabune ) {
                                    balarie[c] = pejos[c];
                                    if (cenusiu[c]) {
                                        int galetusa = p_aleabune[c * nr_aleabune + p_poteca[r]];
                                        if (galetusa > 0) {
                                            if (!rosiatic[c]) { 
                                                toatecele = galetusa == calare[c];
                                                if (toatecele) {
                                                    balarie[c] = galetusa;
                                                }
                                            }
                                        }
                                    }
                                    c++;
                                }
                                if (toatecele) {
                                    for (int r = 0; r < nr_alearele; r++) {
                                        if (!palarie[r]) {
                                            Rboolean nerod = TRUE;
                                            int c = 0;
                                            while (nerod && c < nc_aleabune) {
                                                nerod = (balarie[c] > 0) ? p_alearele[c * nr_alearele + r] + 1 == balarie[c] : TRUE;
                                                c++;
                                            }
                                            palarie[r] = nerod;
                                        }
                                    }
                                }
                            }
                        }
                    }
                } 
            } 
            int toatealea = 0;
            for (int r = 0; r < nr_alearele; r++) {
                toatealea += palarie[r] * 1;
            }
            SEXP ec;
            SET_VECTOR_ELT(EClist, mirare, ec = allocMatrix(INTSXP, toatealea, nc_aleabune));
            int *p_ec = INTEGER(ec);
            SEXP incepute;
            SET_VECTOR_ELT(usage, 4, incepute = VECTOR_ELT(getAttrib(alearele, R_DimNamesSymbol), 0));
            SEXP lafinal;
            SET_VECTOR_ELT(usage, 5, lafinal = allocVector(STRSXP, toatealea)); 
            SEXP dimnames;
            SET_VECTOR_ELT(usage, 0, dimnames = allocVector(VECSXP, 2));
            int ecr = 0;
            for (int r = 0; r < nr_alearele; r++) {
                if (palarie[r]) {
                    for (int c = 0; c < nc_aleabune; c++) {
                        p_ec[c * toatealea + ecr] = p_alearele[c * nr_alearele + r];
                    }
                    SET_STRING_ELT(lafinal, ecr, STRING_ELT(incepute, r));
                    ecr++;
                }
            }
            SET_VECTOR_ELT(dimnames, 0, duplicate(lafinal));
            if (hasColnames(veverita)) {
                SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(getAttrib(veverita, R_DimNamesSymbol), 1));
            }
            setAttrib(ec, R_DimNamesSymbol, dimnames);
            mirare++;
        } 
    } 
    UNPROTECT(2);
    return(EClist);
}
