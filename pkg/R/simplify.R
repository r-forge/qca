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

`simplify` <- function(expression, snames = "", noflevels = NULL, use.tilde = FALSE) {
    syscalls <- unlist(lapply(sys.calls(), deparse))
    if (any(withdata <- grepl("with\\(", syscalls))) {
        snames <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
    }
    snames <- splitstr(snames)
    multivalue <- any(grepl("[{|}]", expression))
    if (multivalue) {
        expression <- toupper(gsub("[*]", "", expression))
        verify.multivalue(expression, snames = snames, noflevels = noflevels) 
    }
    sl <- ifelse(identical(snames, ""), FALSE, ifelse(all(nchar(snames) == 1), TRUE, FALSE))
    if (!grepl("[+]", expression) & grepl("[,]", expression)) {
        if (multivalue) {
            values <- curlyBrackets(expression)
            atvalues <- paste("@", seq(length(values)), sep = "")
            for (i in seq(length(values))) {
                expression <- gsub(values[i], atvalues[i], expression)
            }
            expression <- gsub(",", "+", expression)
            for (i in seq(length(values))) {
                expression <- gsub(atvalues[i], values[i], expression)
            }
        }
        else {
            oldway <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", expression), split = ""))
            if (!possibleNumeric(oldway) & length(oldway) > 0) {
                expression <- gsub(",", "+", expression)
            }
        }
    }
    getbl <- function(expression) {
        bl <- splitMainComponents(gsub("[[:space:]]", "", expression))
        bl <- splitBrackets(bl)
        bl <- removeSingleStars(bl)
        bl <- splitPluses(bl)
        blu <- unlist(bl)
        bl <- splitStars(bl, ifelse((sl | any(hastilde(blu) & !tilde1st(blu))) & !grepl("[*]", expression) & !multivalue, "", "*"))
        bl <- solveBrackets(bl)
        bl <- simplifyList(bl)
        return(bl)
    }
    checksubset <- function(mat) {
        for (i in 1:2) {
            eqz <- mat[i, ] == 0
            if (nrow(unique(mat[, !eqz, drop = FALSE])) == 1) {
                return(3 - i)
            }
        }
        return(NULL)
    }
    qmc <- function(implicants, nrowexp) {
        noflevels <- rep(2, ncol(implicants))
        if (nrowexp == 1) {
            return(implicants[1, , drop = FALSE])
        }
        minimized <- rep(TRUE, nrow(implicants))
        first <- nrow(implicants) > nrowexp
        if (first) {
            minimized[seq(nrowexp)] <- FALSE
        }
        else {
            minimized <- TRUE
        }
        while (any(minimized) & nrow(implicants) > 1) {
            if (first) {
                irows <- seq(nrowexp)
            }
            else {
                minimized <- logical(nrow(implicants))
                irows <- seq(nrow(implicants) - 1)
            }
            tbc <- matrix(nrow = 0, ncol = 2)
            for (i in irows) {
                for (j in seq(i + 1, nrow(implicants))) {
                    if (sum(implicants[i, ] != implicants[j, ]) == 1) {
                        tbc <- rbind(tbc, c(i, j))
                    }
                }
            }
            first <- FALSE
            result <- NULL
            if (nrow(tbc) > 0) {
                differences <- t(apply(tbc, 1, function(idx) implicants[idx[1], ] != implicants[idx[2], ]))
                result <- matrix(nrow = 0, ncol = ncol(differences))
                for (i in seq.int(nrow(differences))) {
                    stable.values <- implicants[tbc[i, 1], !differences[i, , drop = FALSE], drop = FALSE]
                    subset.explain <- apply(implicants[, !differences[i, , drop = FALSE], drop = FALSE], 1, function(x) all(x == stable.values))
                    if (sum(subset.explain) == noflevels[differences[i, ]]) {
                        minimized[subset.explain] <- TRUE
                        minimization.result <- implicants[tbc[i, 1], , drop = FALSE]
                        minimization.result[differences[i, ]] <- 0
                        result <- rbind(result, as.vector(minimization.result))
                    }
                }
            }
            if (sum(minimized) > 0) {
                implicants <- implicants[!minimized, ]
                if (!is.null(result)) {
                    implicants <- rbind(implicants, unique(result))
                }
            }
        }
        return(implicants)
    }
    bl <- list()
    if (any(hastilde(expression))) {
        use.tilde <- TRUE
    }
    for (i in seq(length(expression))) {
        bl <- c(bl, lapply(getbl(expression[i]), function(x) {
            x <- unlist(x)
            if (multivalue) {
                outx <- toupper(curlyBrackets(x, outside=TRUE))
                inx <- curlyBrackets(x)
                x <- paste(outx, "{", inx, "}", sep = "")
            }
            x <- cx <- unique(unlist(x))
            tx <- which(hastilde(x))
            if (!multivalue) {
                if (any(tx)) {
                    x <- notilde(x)
                    uptx <- is.element(x[tx], toupper(x))
                    lotx <- is.element(x[tx], tolower(x))
                    x[tx[uptx]] <- tolower(x[tx[uptx]])
                    x[tx[lotx]] <- toupper(x[tx[lotx]])
                }
            }
            cx <- cx[!duplicated(x)]
            if (any(duplicated(toupper(notilde(cx))))) {
                return(NULL)
            }
            else {
                if (use.tilde) {
                    tx <- hastilde(cx)
                    x <- notilde(cx)
                    lotx <- is.element(x, tolower(x))
                    tx[lotx] <- !tx[lotx]
                    x <- toupper(x)
                    x[tx] <- paste("~", x[tx], sep = "")
                    cx <- x
                }
                return(cx)
            }
        }))
    }
    bl <- unique(bl[!unlist(lapply(bl, is.null))])
    bl <- paste(unlist(lapply(bl, paste, collapse = "*")), collapse = " + ")
    if (identical(bl, "")) {
        return(bl)
    }
    tlist <- list(expression = bl, snames = snames)
    if (!is.null(noflevels)) tlist$noflevels <- noflevels
    bl <- do.call("translate", tlist)
    expressions <- matrix(nrow = 0, ncol = ncol(bl))
    colnames(expressions) <- colnames(bl)
    mint <- rownames(bl) 
    for (i in seq(nrow(bl))) {
        expressions <- rbind(expressions, as.matrix(expand.grid(lapply(bl[i, ], function(x) {
            asNumeric(splitstr(x)) + 1
        }))))
    }
    `unite` <- function(x) {
        apply(x, 2, function(x) {
            if (all(x != 0)) {
                if (max(x) - min(x) == 1) {
                    return(0)
                }
                else { 
                    return(x[1]) 
                }
            }
            else {
                return(max(x))
            }
        })
    }
    if (!multivalue) {
        nrowexp <- nrow(expressions)
        if (nrowexp > 1) {
            combs <- combinations(nrowexp, 2)
            for (i in seq(ncol(combs))) {
                submat <- expressions[combs[, i], ]
                ints <- apply(submat, 2, function(x) all(as.logical(x)))
                if (any(ints)) {
                    sums <- apply(submat, 1, function(x) sum(as.logical(x)))
                    notequals <- apply(submat, 2, function(x) x[1] != x[2])
                    if (max(sums) - min(sums) == 1 & sum(notequals) == 2) {
                        expressions <- rbind(expressions, apply(submat, 2, function(x) {
                            if (any(x == 0)) {
                                return(max(x))
                            }
                            else {
                                return(x[which.min(sums)])
                            }
                        }))
                    }
                }
            }
            expressions <- qmc(expressions, nrowexp)
        }
    }
    if (all(expressions == 0)) {
        cat("\n")
        stop(simpleError("The result is an empty set.\n\n"))
    }
    if (nrow(expressions) > 1) {
        minimized <- logical(nrow(expressions))
        for (i in seq(nrow(expressions) - 1)) {
            if (!minimized[i]) {
                for (j in seq(i + 1, nrow(expressions))) {
                    if (!minimized[j]) {
                        subsetrow <- checksubset(expressions[c(i, j), , drop = FALSE])
                        if (!is.null(subsetrow)) {
                            minimized[c(i, j)[subsetrow]] <- TRUE
                        }
                    }
                }
            }
        }
        expressions <- expressions[!minimized, , drop = FALSE]
    }
    expressions <- writePrimeimp(sortExpressions(expressions),
                                 mv = multivalue, use.tilde = use.tilde)
    if (sl) {
        expressions <- gsub("[*]", "", expressions)
    }
    mlist <- list(primes = expressions, configs = mint)
    if (!is.null(noflevels)) mlist$noflevels <- noflevels
    if (!identical(snames, "")) mlist$snames <- snames
    return(paste(expressions, collapse = " + "))
}
`sop` <- function(...) {
    .Deprecated(msg = "Function sop() is deprecated, and has been renamed to simplify()\n")
    simplify(...)
}
