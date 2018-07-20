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

`fuzzyand` <- function(..., na.rm = FALSE, use.tilde = FALSE) {
    funargs <- unlist(lapply(lapply(match.call(), deparse)[-1], function(x) gsub("\"|[[:space:]]", "", x)))
    if (!is.na(rem <- match("use.tilde", names(funargs)))) {
        funargs <- funargs[-rem]
    }
    if (!is.na(rem <- match("na.rm", names(funargs)))) {
        funargs <- funargs[-rem]
    }
    other.args <- vector(mode = "list", length = length(funargs))
    funargs <- gsub(rawToChar(as.raw(c(226, 128, 147))), "-", funargs)
    negated <- grepl("1-", funargs)
    funargs <- gsub("1-", "", funargs)
    tildenegated <- badnames <- cols <- logical(length(funargs))
    for (i in seq(length(funargs))) {
        badnames[i] <- grepl("\\(|:", funargs[i])
        cols[i] <- getName(notilde(funargs[i]))
        tildenegated[i] <- tilde1st(funargs[i])
        funargs[i] <- notilde(funargs[i])
    }
    if (sum(badnames) > 0) {
        if (sum(badnames) > length(LETTERS) | any(is.element(cols, LETTERS))) {
            cols[badnames] <- paste("X", seq(sum(badnames)), sep = "")
        }
        else {
            cols[badnames] <- LETTERS[seq(sum(badnames))]
        }
    }
    for (i in seq(length(funargs))) {
        tc <- tryCatch(eval.parent(parse(text = funargs[i])), error = function(e) e, warning = function(w) w)
        tc <- capture.output(tc)[1]
        if (identical(substring(gsub("[[:space:]]", "", tc), 1, 9), "function(")) {
            tc <- simpleError("simpleError")
        }
        if (grepl("simpleError", tc)) {
            tc <- tryCatch(eval.parent(parse(text = toupper(funargs[i]))), error = function(e) e, warning = function(w) w)
            tc <- capture.output(tc)[1]
            if (identical(substring(gsub("[[:space:]]", "", tc), 1, 9), "function(")) {
                tc <- simpleError("simpleError")
            }
            if (grepl("simpleError", tc)) {
                cat("\n")
                stop(simpleError(sprintf("Object '%s' not found.\n\n", funargs[i])))
            }
            else {
                negated[i] <- !negated[i]
                other.args[[i]] <- eval.parent(parse(text = toupper(funargs[i])), n = 1)
            }
        }
        else {
            other.args[[i]] <- eval.parent(parse(text = funargs[i]), n = 1)
        }
    }
    if (is.element("name", names(attributes(other.args[[1]])))) {
        other.args[[1]] <- as.vector(other.args[[1]])
    }
    if (is.vector(other.args[[1]])) {
        if (any(!unlist(lapply(other.args, function(x) is.numeric(x) | is.logical(x))))) {
            cat("\n")
            stop(simpleError("Input vectors should be numeric or logical.\n\n"))
        }
        other.args <- as.data.frame(other.args)
    }
    else if (is.matrix(other.args[[1]])) {
        other.args <- other.args[[1]]
        if (is.null(colnames(other.args))) {
            if (ncol(other.args) > length(LETTERS)) {
                cols <- paste("X", seq(ncol(other.args)), sep = "")
            }
            else {
                cols <- LETTERS[seq(ncol(other.args))]
            }
        }
        other.args <- as.data.frame(other.args)
        negated <- logical(ncol(other.args))
        tildenegated <- logical(ncol(other.args))
        if (!all(unlist(lapply(other.args, function(x) is.numeric(x) | is.logical(x))))) {
            cat("\n")
            stop(simpleError("Input should be numeric or logical.\n\n"))
        }
    }
    else if (is.data.frame(other.args[[1]])) {
        other.args <- other.args[[1]]
        negated <- logical(ncol(other.args))
        tildenegated <- logical(ncol(other.args))
        cols <- colnames(other.args)
        if (!all(unlist(lapply(other.args, function(x) is.numeric(x) | is.logical(x))))) {
            cat("\n")
            stop(simpleError("Some columns are not numeric or logical.\n\n"))
        }
    }
    else {
        cat("\n")
        stop(simpleError("The input should be vectors, or a matrix or a dataframe.\n\n"))
    }
    if (any(unlist(lapply(other.args, function(x) any(as.numeric(x) < 0 | as.numeric(x) > 1))))) {
        cat("\n")
        stop(simpleError("Input should be logical or numbers between 0 and 1.\n\n"))
    }
    for (i in seq(length(cols))) {
        if (tildenegated[i]) {
            other.args[[i]] <- 1 - other.args[[i]]
        }
        if (negated[i]) {
            other.args[[i]] <- 1 - other.args[[i]]
        }
        if (negated[i] + tildenegated[i] == 1) {
            if (use.tilde | tildenegated[i] | !identical(cols[i], toupper(cols[i]))) {
                cols[i] <- paste("~", cols[i], sep = "")
            }
            else {
                cols[i] <- tolower(cols[i])
            }
        }
    }
    result <- apply(other.args, 1, min, na.rm = na.rm)
    attr(result, "names") <- NULL
    attr(result, "name") <- paste(cols, collapse = "*")
    class(result) <- c("numeric", "fuzzy")
    return(result)
}
