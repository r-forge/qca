# Copyright (c) 2018, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`getSolution` <-
function(expressions, mv, use.tilde, collapse, inputt, row.dom, initial, all.sol, indata, ...) {
    mtrx <- NULL
    sol.matrix <- NULL
    other.args <- list(...)
    enter <- ifelse (is.element("enter", names(other.args)), other.args$enter, TRUE)
    if (is.list(expressions)) {
        mtrx <- expressions[[2]]
        sol.matrix <- expressions[[3]]
        if (is.null(sol.matrix)) {
            if (enter) cat("\n")
            stop(simpleError(paste("There are no solutions, given these constraints.", ifelse(enter, "\n\n", ""))))
        }
        else {
            sol.matrix[sol.matrix == 0] <- NA
        }
        expressions <- expressions[[1]]
    }
    if (FALSE) {
    if (!missing(indata)) {
        hastime <- logical(ncol(expressions))
        for (i in seq(ncol(expressions))) {
            if (any(indata[, i] %in% c("-", "dc", "?"))) {
                hastime[i] <- TRUE
            }
        }
        indata <- indata[, !hastime, drop = FALSE]
        expressions <- expressions[, !hastime, drop = FALSE]
        inputt <- inputt[, !hastime, drop = FALSE]
        relevant <- apply(expressions, 1, sum) > 0
        if (any(!relevant)) {
            sol.matrix <- NULL
            mtrx <- mtrx[relevant, , drop = FALSE]
            expressions <- expressions[relevant, , drop = FALSE]
        }
    }
    }
    PI <- writePrimeimp(expressions, mv = mv, use.tilde = use.tilde, collapse = collapse)
    rownames(expressions) <- PI
    if (is.null(mtrx)) {
        mtrx <- makeChart(expressions, inputt, mv = mv, use.tilde = use.tilde, collapse = collapse)
    }
    else {
        rownames(mtrx) <- PI
    }
    notempty <- apply(mtrx, 1, any)
        expressions <- expressions[notempty, , drop = FALSE]
        mtrx <- mtrx[notempty, , drop = FALSE]
    setColnames(mtrx, rownames(inputt)) 
    reduced <- list(expressions = expressions, mtrx = mtrx)
    if (nrow(mtrx) > 0) {
        if (row.dom & is.null(sol.matrix)) {
            reduced.rows <- rowDominance(mtrx)
            if (length(reduced.rows) > 0) {
                reduced$mtrx <- mtrx[reduced.rows, , drop=FALSE]
                reduced$expressions <- expressions[reduced.rows, , drop=FALSE]
            }
            sol.matrix <- NULL 
        }
        mtrx <- reduced$mtrx
        setColnames(mtrx, initial)
        if (is.null(sol.matrix)) {
            if (nrow(mtrx) > 150 & nrow(mtrx) * ncol(mtrx) > 1500) {
                message(sprintf("Starting to search all possible solutions in a PI chart with %d rows and %d columns.\nThis will take some time...", nrow(mtrx), ncol(mtrx)))
            }
            sol.matrix <- solveChart(mtrx, all.sol = all.sol, ...=...)
        }
        tokeep <- sort(unique(as.vector(unique(sol.matrix))))
        all.PIs <- rownames(mtrx)[tokeep]
        sol.matrix <- matrix(rownames(mtrx)[sol.matrix], nrow = nrow(sol.matrix))
        reduced$expressions <- reduced$expressions[tokeep, , drop=FALSE]
        solution.list <- writeSolution(sol.matrix, mtrx)
    }
    else {
        all.PIs <- NA
        solution.list <- NA
    }
    return(list(mtrx=mtrx, reduced=reduced, expressions=expressions, all.PIs=all.PIs, solution.list=solution.list))
}
