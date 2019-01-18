# Copyright (c) 2019, Adrian Dusa
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

`intersection` <- function(..., snames = "", use.tilde = FALSE, noflevels) {
    allargs <- list(...)
    if (length(allargs) == 0) {
        cat("\n")
        stop(simpleError("Nothing to intersect.\n\n"))
    }
    snames <- splitstr(snames)
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    isol <- NULL
    for (i in seq(length(allargs))) {
        x <- allargs[[i]]
        if (methods::is(allargs[[i]], "qca")) {
            if (identical(snames, "")) {
                snames <- allargs[[i]]$tt$options$conditions
                if (allargs[[i]]$options$use.letters) {
                    snames <- LETTERS[seq(length(snames))]
                }
            }
            use.tilde <- allargs[[i]]$options$use.tilde
            if ("i.sol" %in% names(x)) {
                elengths <- unlist(lapply(allargs[[i]]$i.sol, function(x) length(x$solution)))
                isol <- paste(rep(names(allargs[[i]]$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
                allargs[[i]] <- as.vector(unlist(lapply(allargs[[i]]$i.sol, function(x) {
                    lapply(x$solution, paste, collapse = " + ")
                })))
            }
            else {
                allargs[[i]] <- as.vector(unlist(lapply(allargs[[i]]$solution, paste, collapse = " + ")))
            }
        }
        else if (methods::is(allargs[[i]], "deMorgan")) {
            isol <- attr(allargs[[i]], "isol")
        }
        if (!is.character(allargs[[i]])) {
            cat("\n")
            stop(simpleError("Unrecognised input.\n\n"))
        }
    }
    arglist <- list(snames = snames, use.tilde = use.tilde)
    if (!missing(noflevels)) {
        arglist$noflevels <- noflevels
    }
    combs <- createMatrix(unlist(lapply(allargs, length)))
    expressions <- result <- character(nrow(combs))
    conj <- ifelse(sl, "", "*")
    for (i in seq(nrow(combs))) {
        x <- combs[i, ] + 1
        expression <- c()
        for (j in seq(length(x))) {
            expression <- c(expression, allargs[[j]][x[j]])
        }
        disj <- grepl("[+]", expression)
        if (any(disj)) {
            expression[disj] <- paste("(", expression[disj], ")", sep = "")
        }
        if (any(!disj)) {
            ndisj <- which(!disj)
            if (any(ndisj == 1)) {
                expression[1] <- paste(expression[1], conj, sep = "")
            }
            if (any(ndisj == length(expression))) {
                expression[length(expression)] <- paste(conj, expression[length(expression)], sep = "")
            }
            if (length(ndisj <- setdiff(ndisj, c(1, length(expression)))) > 0) {
                expression[ndisj] <- paste(conj, expression[ndisj], conj, sep = "")
            }
        }
        expressions[i] <- paste(expression, collapse = "")
        expressions[i] <- gsub("\\*\\(", "(", expressions[i])
        result[i] <- do.call("simplify", c(list(expressions[i]), arglist))
    }
    if (sl) {
        for (i in seq(length(expressions))) {
            result[i] <- gsub("[*]", "", result[i])
        }
    }
    attr(result, "expressions") <- expressions
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    class(result) <- c("character", "intersection")
    return(result)
}
