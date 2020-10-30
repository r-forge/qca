/*
Copyright (c) 2020, Adrian Dusa
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "find_min.h"
#include "consistency.h"
#include "find_models.h"
#include "find_consistent_models.h"
#include "CCubes.h"
#ifdef _OPENMP
  #include <omp.h>
#endif
void CCubes(const int p_tt[],       
            const int ttrows,       
            const int nconds,       
            const double p_data[],  
            const int nrdata,       
            const bool allsol,      
            const bool rowdom,      
            const bool minpin,      
            const double picons,    
            const int pidepth,      
            const int p_fsconds[],  
            const int soldepth,     
            const double solcons,
            const double solcov,
            const double maxcomb,
            const bool keeptrying,  
            int **pichart,          
            int **implicants,       
            int **models,           
            int *foundPI_,          
            int *solrows,           
            int *solcols,           
            bool *complex,          
            const bool firstmin)    
{
    int *p_pichart, *p_implicants, *p_indx, *p_ck;
    int posrows = 0;
    for (int r = 0; r < ttrows; r++) {
        posrows += p_tt[nconds * ttrows + r];
    }
    int negrows = ttrows - posrows;
    int posmat[posrows * nconds];
    int negmat[nconds * ((negrows > 0) ? negrows : 1)]; 
    int rowpos = 0, rowneg = 0;
    populate_posneg(&rowpos, &rowneg, nconds, ttrows, posrows, p_tt, posmat, negmat);
    int noflevels[nconds];
    for (int i = 0; i < nconds; i++) {
        noflevels[i] = 2; 
    }
    get_noflevels(noflevels, p_tt, nconds, ttrows);
    int estimPI = 10000;
    p_pichart = malloc(posrows * estimPI * sizeof(int));
    memset(p_pichart, false, posrows * estimPI * sizeof(int));
    p_implicants = calloc(nconds * estimPI, sizeof(int));
    p_indx = calloc(nconds * estimPI, sizeof(int));
    p_ck = calloc(estimPI, sizeof(int));
    bool stop_searching = false;
    int prevfoundPI = 0;    
    int foundPI = 0;
    int prevsolmin = 0;     
    int solmin = 0;
    int previndices[posrows];
    int indices[posrows];
    for (int i = 0; i < posrows; i++) {
        previndices[i] = 0;
        indices[i] = 0;
    }
    #ifdef SHOW_DEBUG_PROFILE 
        const double findingPIsStart_time = omp_get_wtime();
    #endif
    bool solution_exists = false;
    int counter = 0; 
    int k;
    for (k = 1; k <= pidepth; k++) {
        bool foundk = false;
        #ifdef SHOW_DEBUG_OUTPUT
        #endif 
        const int numMaxCombinations = (nconds*(nconds-1)*(nconds-2))/6 + (nconds*(nconds-1)/2) + nconds;
        int combinationsSetFixedPrefix[numMaxCombinations][3];
        int numPrefixCombinations = 0;
	    
        const int INDEX_LIMIT = fillCombinationTasks(nconds, k, combinationsSetFixedPrefix, numMaxCombinations, &numPrefixCombinations);
        #ifdef _OPENMP
                #pragma omp parallel for schedule(dynamic)
        #endif
        for (int taskIter = 0; taskIter < numPrefixCombinations; taskIter++) {
            int tempk[k];
            for (int i = 0; i < INDEX_LIMIT; i++) {
                tempk[i] = combinationsSetFixedPrefix[taskIter][i];
            }
            for (int i = INDEX_LIMIT; i < k; i++) {
                tempk[i] = tempk[i - 1] + 1;
            }
            if (k > INDEX_LIMIT)
                tempk[k - 1]--; 
            int decpos[posrows];
            int decneg[(negrows > 0) ? negrows : 1]; 
            int finishedAll = 0;
            int mbase[k];
            mbase[0] = 1; 
            while (!finishedAll) {
                if (k > INDEX_LIMIT) {
                    int res = GetNextComb(tempk, k, nconds, INDEX_LIMIT);
                    if (!res) {
                        finishedAll = 1;
                        break; 
                    }                    
                }
                else {
                    finishedAll = 1;
                }
                #ifdef SHOW_DEBUG_OUTPUT
                    #pragma omp critical
                    {
                        int threadid = omp_get_thread_num();
                    }
                #endif
                fill_mbase(mbase, tempk, noflevels, k);
                get_decimals(posrows, negrows, k, decpos, decneg, posmat, negmat, tempk, mbase);
                int possiblePIrows[posrows];
                possiblePIrows[0] = 0; 
                bool possiblePI[posrows];
                possiblePI[0] = true; 
                int found = 1;
                get_uniques(posrows, &found, decpos, possiblePI, possiblePIrows);
                int compare = found;
                if (picons > 0) {
                    int val[k];
                    int fuzzy[k];
                    for (int i = 0; i < compare; i++) {
                        for (int c = 0; c < k; c++) {
                            val[c] = posmat[tempk[c] * posrows + possiblePIrows[i]];
                            fuzzy[c] = p_fsconds[tempk[c]] * 1;
                        }
                        if (altb(consistency(p_data, nrdata, nconds, k, tempk, val, fuzzy), picons)) {
                            possiblePI[i] = false;
                            found -= 1;
                        }
                    }
                }
                else if (negrows > 0) {
                    verify_possible_PI(compare, negrows, &found, possiblePI, possiblePIrows, decpos, decneg);
                }
                if (found) { 
                    int frows[found];
                    get_frows(frows, possiblePI, possiblePIrows, compare);
                    for (int f = 0; f < found; f++) {
                        int tempc[k];
                        for (int c = 0; c < k; c++) {
                            tempc[c] = posmat[tempk[c] * posrows + frows[f]] + 1;
                        }
                        if (nonredundant(p_implicants, p_indx, p_ck, tempk, tempc, nconds, k, prevfoundPI)) {
                            #ifdef _OPENMP    
                                #pragma omp critical
                            #endif 
                            {
                                push_PI(p_implicants, p_indx, p_ck, p_pichart, tempk, tempc, nconds, k, f, decpos, frows, posrows, foundPI);
                                ++foundPI;
                                foundk = true;
                                if (foundPI / estimPI > 0.9) {
                                    estimPI += 10000;
                                    resize(&p_pichart,    posrows, estimPI, foundPI);
                                    resize(&p_implicants, nconds,  estimPI, foundPI);
                                    resize(&p_indx,       nconds,  estimPI, foundPI);
                                    resize(&p_ck,         1,       estimPI, foundPI);
                                }
                            }
                        }
                    }
                }
            }
        }
        if (foundPI > 0) {
            *complex = too_complex(foundPI, (solmin > 0 ? solmin : k), maxcomb);
            solution_exists = all_covered(p_pichart, posrows, foundPI);
            if (solution_exists) {
                stop_searching = *complex;
            }
            if (!stop_searching && solution_exists) { 
                find_min(p_pichart, posrows, foundPI, &solmin, indices); 
                if (solmin == prevsolmin) {
                    if (firstmin || minpin) {
                        foundPI = prevfoundPI;
                        for (int i = 0; i < solmin; i++) {
                            indices[i] = previndices[i];
                        }
                        stop_searching = true; 
                    }
                }
                else {
                    prevsolmin = solmin;
                    for (int i = 0; i < solmin; i++) {
                        previndices[i] = indices[i];
                    }
                }
                if (foundk) {
                    counter = 0;
                }
                else {
                    counter += 1;
                }
            }
            prevfoundPI = foundPI;
        }
        if (stop_searching || counter > 1) {
            break;
        }
    }
    #ifdef SHOW_DEBUG_PROFILE
        const double findingPIsEnd_time = omp_get_wtime();
    #endif
    int *copy_implicants = calloc(1, sizeof(int));
    int *p_solutions = calloc(1, sizeof(int));
    int nr = 0, nc = 0; 
    int *p_tempic = calloc(1, sizeof(int));
    if ((firstmin || *complex) && solcons == 0) {
        if (solmin > 0) {
            free(copy_implicants);
            copy_implicants = malloc(nconds * solmin * sizeof(int));
            free(p_solutions);
            p_solutions = malloc(solmin * sizeof(int));
            free(p_tempic);
            p_tempic = malloc(posrows * solmin * sizeof(int));
            for (int c = 0; c < solmin; c++) {
                p_solutions[c] = indices[c];
                for (int r = 0; r < nconds; r++) {
                    copy_implicants[c * nconds + r] = p_implicants[indices[c] * nconds + r];
                }
                for (int r = 0; r < posrows; r++) {
                    p_tempic[c * posrows + r] = p_pichart[indices[c] * posrows + r];
                }
            }
            nr = solmin;
            nc = 1;
            foundPI = solmin;
        }
        else if (foundPI > 0) { 
            int *p_sorted = malloc(foundPI * sizeof(int));
            for (int i = 0; i < foundPI; i++) {
                p_sorted[i] = i;
            }
            sort_cols(p_implicants, p_sorted, p_ck, nconds, foundPI);
            free(copy_implicants);
            copy_implicants = malloc(nconds * foundPI * sizeof(int));
            free(p_tempic);
            p_tempic = malloc(posrows * foundPI * sizeof(int));
            for (int c = 0; c < foundPI; c++) {
                for (int r = 0; r < nconds; r++) {
                    copy_implicants[c * nconds + r] = p_implicants[p_sorted[c] * nconds + r];
                }
                for (int r = 0; r < posrows; r++) {
                    p_tempic[c * posrows + r] = p_pichart[p_sorted[c] * posrows + r];
                }
            }
            free(p_sorted);
        }
    }
    else if (foundPI > 0) { 
        if (rowdom) {
            row_dominance(p_pichart, p_implicants, p_ck, posrows, &foundPI, nconds);
        }
        int *p_sorted = malloc(foundPI * sizeof(int));
        for (int i = 0; i < foundPI; i++) {
            p_sorted[i] = i;
        }
        sort_cols(p_implicants, p_sorted, p_ck, nconds, foundPI);
        free(copy_implicants);
        copy_implicants = calloc(nconds * foundPI, sizeof(int));
        free(p_tempic);
        p_tempic = calloc(posrows * foundPI, sizeof(int));
        for (int c = 0; c < foundPI; c++) {
            for (int r = 0; r < nconds; r++) {
                copy_implicants[c * nconds + r] = p_implicants[p_sorted[c] * nconds + r];
            }
            for (int r = 0; r < posrows; r++) {
                p_tempic[c * posrows + r] = p_pichart[p_sorted[c] * posrows + r];
            }
        }
        if (solcons > 0) {
            int *p_tempindx = calloc(posrows * foundPI, sizeof(int));
            int *p_tempck = calloc(foundPI, sizeof(int));
            for (int c = 0; c < foundPI; c++) {
                for (int r = 0; r < nconds; r++) {
                    p_tempindx[c * nconds + r] = p_indx[p_sorted[c] * nconds + r];
                }
                p_tempck[c] = p_ck[p_sorted[c]];
            }
            find_consistent_models(copy_implicants, p_tempindx, p_tempck, p_data, p_fsconds, nconds, nrdata, posrows, solcons, solcov, allsol, soldepth, foundPI, maxcomb, &p_solutions, &nr, &nc);
            free(p_tempindx);
            free(p_tempck);
        }
        else if (solmin > 0) {
            nr = solmin;
            find_models(p_tempic, posrows, foundPI, allsol, solmin, maxcomb, false, &p_solutions, &nr, &nc);
        }
        free(p_sorted);
    }
    free(p_pichart);
    free(p_implicants);
    free(p_indx);
    free(p_ck);
    free(*models);
    free(*implicants);
    free(*pichart);
    *solrows = nr;
    *solcols = nc;
    *foundPI_ = foundPI;
    *implicants = copy_implicants;
    *pichart = p_tempic;
    *models = p_solutions;
    return;
}
