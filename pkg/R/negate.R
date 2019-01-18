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

`negate` <-
function(expression, snames = "", noflevels, use.tilde = FALSE) {
    if (!missing(noflevels)) {
        noflevels <- splitstr(noflevels)
    }
    isol <- NULL
    if (methods::is(expression, "qca")) {
        snames <- expression$tt$options$conditions
        if (expression$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
        }
        noflevels <- expression$tt$noflevels
        if ("i.sol" %in% names(expression)) {
            elengths <- unlist(lapply(expression$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(expression$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            expression <- unlist(lapply(expression$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
        }
        else {
            expression <- unlist(lapply(expression$solution, paste, collapse = " + "))
        }
    }
    if (is.character(expression)) {
        star <- any(grepl("[*]", expression))
        if (any(hastilde(expression))) {
            use.tilde <- TRUE
        }
        mv <- any(grepl("[{|}]", expression))
        negateit <- function(x, snames, noflevels) {
            x <- simplify(x, snames = snames, noflevels = noflevels)
            trexp <- translate(x, snames = snames, noflevels = noflevels)
            snames <- colnames(trexp)
            if (missing(noflevels)) {
                noflevels <- rep(2, ncol(trexp))
            }
            snoflevels <- lapply(noflevels, function(x) seq(x) - 1)
            negated <- paste(apply(trexp, 1, function(x) {
                wx <- which(x != -1) 
                x <- x[wx]
                nms <- names(x)
                x <- sapply(seq_along(x), function(i) {
                    paste(setdiff(snoflevels[wx][[i]], splitstr(x[i])), collapse = ",")
                })
                if (mv) {
                    return(paste("(", paste(nms, "{", x, "}", sep = "", collapse = " + "), ")", sep = ""))
                }
                else {
                    nms[x == 0] <- tolower(nms[x == 0])
                    return(paste("(", paste(nms, collapse = " + ", sep = ""), ")", sep = ""))
                }
            }), collapse = "")
            negated <- simplify(negated, snames = snames, noflevels = noflevels)
            if (use.tilde & !mv) {
                trneg <- translate(negated, snames = snames, noflevels = noflevels)
                negated <- paste(apply(trneg, 1, function(x) {
                    wx <- which(x >= 0)
                    x <- x[wx]
                    nms <- names(x)
                    nms[x == 0] <- paste("~", nms[x == 0], sep = "")
                    return(paste(nms, collapse = "*"))
                }), collapse = " + ")
            }
            if (!star) {
                negated <- gsub("[*]", "", negated)
            }
            return(negated)
        }
        result <- unlist(lapply(expression, negateit, snames = snames, noflevels = noflevels))
        attr(result, "expressions") <- expression
        if (!identical(snames, "")) {
            attr(result, "snames") <- snames
        }
        if (!is.null(isol)) {
            attr(result, "isol") <- isol
        }
        class(result) <- c("character", "deMorgan")
        return(result)
    }
    else {
        cat("\n")
        stop(simpleError("The expression should be a character vector.\n\n"))
    }
}
`deMorgan` <- function(...) {
    .Deprecated(msg = "Function deMorgan() is deprecated. Use function negate() instead.\n")
    negate(...)
}
