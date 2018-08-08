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

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> 
#include <R_ext/Rdynload.h>
extern SEXP ccubes(SEXP);
extern SEXP combinations(SEXP);
extern SEXP createMatrix(SEXP);
extern SEXP findmin(SEXP);
extern SEXP findSubsets(SEXP, SEXP, SEXP, SEXP);
extern SEXP getRow(SEXP);
extern SEXP QMC(SEXP, SEXP);
extern SEXP removeRedundants(SEXP, SEXP, SEXP);
extern SEXP setColnames(SEXP, SEXP);
extern SEXP setDimnames(SEXP, SEXP);
extern SEXP setRownames(SEXP, SEXP);
extern SEXP solveChart(SEXP, SEXP, SEXP);
extern SEXP superSubset(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP truthTable(SEXP, SEXP, SEXP, SEXP);
static const R_CallMethodDef CallEntries[] = {
    {"ccubes",           (DL_FUNC) &ccubes,           1},
    {"combinations",     (DL_FUNC) &combinations,     1},
    {"createMatrix",     (DL_FUNC) &createMatrix,     1},
    {"findmin",          (DL_FUNC) &findmin,          1},
    {"findSubsets",      (DL_FUNC) &findSubsets,      4},
    {"getRow",           (DL_FUNC) &getRow,           1},
    {"QMC",              (DL_FUNC) &QMC,              2},
    {"removeRedundants", (DL_FUNC) &removeRedundants, 3},
    {"setColnames",      (DL_FUNC) &setColnames,      2},
    {"setDimnames",      (DL_FUNC) &setDimnames,      2},
    {"setRownames",      (DL_FUNC) &setRownames,      2},
    {"solveChart",       (DL_FUNC) &solveChart,       3},
    {"superSubset",      (DL_FUNC) &superSubset,      8},
    {"truthTable",       (DL_FUNC) &truthTable,       4},
    {NULL, NULL, 0}
};
void R_init_QCA(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
