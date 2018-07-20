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

# include <R.h>
# include <Rinternals.h>
# include <R_ext/Rdynload.h>
# include <stdlib.h>
SEXP findSubsets(SEXP rowno, SEXP noflevels, SEXP mbase, SEXP max) {
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
