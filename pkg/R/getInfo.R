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

`getInfo` <- function(data, conditions = "", outcome = "") {
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }
    if (identical(outcome, "")) {
        if (identical(conditions, "")) {
            conditions <- colnames(data)
        }
        else {
            if (is.null(colnames(data))) {
                cat("\n")
                stop(simpleError("Unspecified column names in the data.\n\n"))
            }
            outcome <- setdiff(colnames(data), conditions)
            if (length(outcome) == 0) {
                cat("\n")
                stop(simpleError("Not enough columns in the data.\n\n"))
            }
            outcome <- outcome[1]
        }
    }
    else {
        if (identical(conditions, "")) {
            conditions <- setdiff(colnames(data), outcome)
        }
    }
    dc.code <- unique(unlist(lapply(data, function(x) {
        if (is.numeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[x %in% c("-", "dc")]))
        }
    })))
    if (length(dc.code) == 0) {
        dc.code <- -1
    }
    else if (length(dc.code) > 1) {
        cat("\n")
        stop(simpleError("Multiple \"Don't care\" codes found.\n\n"))
    }
    colnms <- colnames(data)
    data <- as.data.frame(lapply(data, function(x) {
        x <- as.character(x)
        x[x == dc.code] <- -1
        return(asNumeric(x))
    }))
    colnames(data) <- colnms
    data[data < 0] <- -1
    fuzzy.cc <- apply(data[, conditions, drop = FALSE], 2, function(x) {
        if (possibleNumeric(x)) {
            return(any(x %% 1 > 0))
        }
        else {
            return(FALSE)
        }
    })
    hastime <- logical(length(conditions))
    for (i in seq(length(conditions))) {
        if (!fuzzy.cc[i] & possibleNumeric(data[, i])) {
            copy.cc <- data[, i]
            if (any(copy.cc < 0)) {
                hastime[i] <- TRUE
                copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                data[, i] <- copy.cc
            }
        }
    }
    noflevels <- apply(data[, conditions, drop = FALSE], 2, max) + 1
    noflevels[noflevels == 1] <- 2
    noflevels[fuzzy.cc] <- 2
    noflevels <- as.integer(noflevels)
    if (length(conditions) == ncol(data)) {
        return(noflevels)
    }
    return(list(data = data, fuzzy.cc = fuzzy.cc, hastime = hastime, dc.code = dc.code, noflevels = as.numeric(noflevels)))
}
