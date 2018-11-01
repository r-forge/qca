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

`pofind` <-
function(data, outcome = "", conditions = "", relation = "necessity",
         use.tilde = FALSE, ...) {
    funargs <- lapply(match.call(), deparse)
    if (missing(data)) {
        cat("\n")
        stop(simpleError("Data is missing.\n\n"))
    }
    if (identical(outcome, "")) {
        cat("\n")
        stop(simpleError("The outcome is missing.\n\n"))
    }
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }
    verify.qca(data)
    for (i in seq(ncol(data))) {
        if (!is.numeric(data[, i])) {
            if (possibleNumeric(data[, i])) {
                data[, i] <- asNumeric(data[, i])
            }
        }
    }
    colnames(data) <- toupper(colnames(data))
    if (!is.element(outcome, c(tolower(outcome), toupper(outcome)))) {
        cat("\n")
        stop(simpleError("The outcome name should not contain both lower and upper case letters.\n\n"))
    }
    origoutcome <- outcome
    outcome <- toupper(notilde(curlyBrackets(outcome, outside = TRUE)))
    if (!is.element(outcome, colnames(data))) {
        cat("\n")
        stop(simpleError("Outcome not found in the data.\n\n"))
    }
    if (identical(conditions, "")) {
        conditions <- setdiff(colnames(data), outcome)
    }
    else {
        conditions <- toupper(splitstr(conditions))
        if (any(!is.element(conditions, colnames(data)))) {
            cat("\n")
            stop(simpleError("Conditions not found in the data.\n\n"))
        }
    }
    data <- data[, c(conditions, outcome)]
    noflevels <- getLevels(data[, conditions, drop = FALSE])
    if (any(noflevels > 2)) { 
        expression <- paste(unlist(lapply(seq(length(conditions)), function(x) {
            values <- sort(unique(data[, conditions[x]]))
            return(paste(conditions[x], "{", values, "}", sep = ""))
        })), collapse = "+")
    }
    else {
        if (use.tilde) {
            negconditions <- paste("~", conditions, sep = "")
        }
        else {
            negconditions <- tolower(conditions)
        }
        expression <- paste(negconditions, conditions, sep = "+", collapse = "+")
    }
    pofargs <- list(setms = expression,
                    outcome = origoutcome,
                    data = data,
                    relation = relation,
                    ... = ...)
    result <- do.call("pof", pofargs)
    result$incl.cov <- result$incl.cov[-nrow(result$incl.cov), , drop = FALSE]
    result$options$setms <- result$options$setms[, -ncol(result$options$setms), drop = FALSE]
    if (is.element("covU", colnames(result$incl.cov))) {
        result$incl.cov <- result$incl.cov[, setdiff(colnames(result$incl.cov), "covU")]
    }
    if (use.tilde) {
        rownames(result$incl.cov)[seq(length(conditions))] <- paste("", rownames(result$incl.cov)[seq(length(conditions))])
    }
    result$options$data <- funargs$data
    return(result)
}
