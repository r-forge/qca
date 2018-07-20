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

`createMatrix` <-
function(noflevels, ...) {
    other.args <- list(...)
    RAM <- 2
    if ("RAM" %in% names(other.args)) {
        if (length(other.args$RAM) == 1) {
            if (is.numeric(other.args$RAM) & other.args$RAM > 0) {
                RAM <- other.args$RAM
            }
        }
    }
    arrange <- FALSE
    if ("arrange" %in% names(other.args)) {
        arrange <- other.args$arrange
    }
    depth <- length(noflevels)
    if ("depth" %in% names(other.args)) {
        if (!is.null(other.args$depth)) {
            if (is.numeric(other.args$depth)) {
                depth <- other.args$depth
                if (depth < length(noflevels)) {
                    arrange <- TRUE
                }
            }
        }
    }
    if (any(abs(noflevels) %% 1 > .Machine$double.eps ^ 0.5)) {
        cat("\n")
        stop(simpleError("The number of levels need to be integers."))
    }
    if (!is.logical(arrange)) {
        cat("\n")
        stop(simpleError("The number of \"arrange\" should be logical."))
    }
    if (abs(depth) %% 1 > .Machine$double.eps ^ 0.5) {
        cat("\n")
        stop(simpleError("The argument depth has to be an integer number."))
    }
    if ((mem <- prod(noflevels)*length(levels)*8/1024^3) > RAM) {
        cat("\n")
        stop(simpleError(paste("Too much memory needed (", round(mem, 1), " Gb) to create the matrix.", sep="")))
    }
    noflevels <- as.integer(abs(noflevels))
    arrange <- as.integer(arrange * 1)
    depth <- as.integer(abs(depth))
    nofconds <- as.integer(length(noflevels))
    if (arrange) {
        if (depth < 1 | depth > nofconds) {
            depth <- nofconds
        }
    }
    tosend <- list(noflevels, arrange, depth)
    if (is.element("colnames", names(other.args))) {
        colnms <- other.args$colnames
        if (is.character(colnms)) {
            if (length(colnms) == length(noflevels)) {
                tosend <- c(tosend, list(colnms))
            }
        }
    }
    return(.Call("createMatrix", tosend, PACKAGE = "QCA"))
    pwr <- unique(noflevels)
    if (length(pwr) == 1) {
        create <- function(idx) {
            rep.int(c(sapply(seq_len(pwr) - 1, function(x) rep.int(x, pwr^(idx - 1)))),
                    pwr^nofconds/pwr^idx)
        }
        retmat <- sapply(rev(seq_len(nofconds)), create)
    }
    else {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        orep  <- cumprod(rev(c(rev(noflevels)[-1], 1)))
        retmat <- sapply(seq_len(nofconds), function(x) {
           rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
        })
    }
    if (is.vector(retmat)) {
        retmat <- matrix(retmat, nrow=1)
    }
    return(retmat)
}
