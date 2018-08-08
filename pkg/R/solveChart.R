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

`solveChart` <-
function(chart, row.dom = FALSE, all.sol = FALSE, depth = NULL, ...) {
    if (!is.logical(chart) && length(setdiff(chart, 0:1)) > 0) {
        cat("\n")
        stop(simpleError("Use a T/F matrix. See makeChart()'s output.\n\n"))
    }
    other.args <- list(...)
    if ("min.dis" %in% names(other.args)) {
        if (is.logical(other.args$min.dis)) {
            all.sol <- !other.args$min.dis
        }
    }
    if (all.sol) {
        row.dom <- FALSE
    }
    row.numbers <- seq(nrow(chart))
    if (row.dom) {
        row.numbers <- rowDominance(chart)
        chart <- chart[row.numbers, ]
    }
    if (findmin(chart) == 0) {
        cat("\n")
        stop(simpleError("The PI chart cannot be solved.\n\n"))
    }
    if (all(dim(chart) > 1)) {
        if (is.null(depth)) depth <- 0L
        output <- .Call("C_solveChart", t(matrix(as.logical(chart), nrow = nrow(chart))), all.sol, as.integer(depth), PACKAGE = "QCA")
        output[output == 0] <- NA
    }
    else {
        output <- matrix(seq(nrow(chart)))
        if (ncol(chart) == 1) {
            output <- t(output)
        }
    }
    return(matrix(row.numbers[output], nrow=nrow(output)))
}
