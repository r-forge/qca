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

`factorize` <- 
function(input, snames = "", noflevels, pos = FALSE, use.tilde = FALSE, ...) {
    other.args <- list(...)
    if (any(names(other.args) == "tilde")) {
        use.tilde <- other.args$tilde
    }
    pasteit <- function(mat, comrows, cols, comvals) {
        if (!missing(cols)) {
            temp <- mat[comrows, -cols, drop = FALSE]
            if (mv) {
                cf <- paste(colnames(mat)[cols], "{", comvals, "}", sep = "")
                rowsf <- lapply(seq(nrow(temp)), function(x) {
                    fname <- colnames(temp)
                    x <- temp[x, ]
                    return(paste(fname, "{", x, "}", sep = "")[x >= 0])
                })
            }
            else if (use.tilde) {
                cf <- paste(ifelse(comvals == 0, "~", ""), colnames(mat)[cols], sep = "")
                rowsf <- lapply(seq(nrow(temp)), function(x) {
                    x <- temp[x, ]
                    return(paste(ifelse(x == 0, "~", ""), names(x), sep = "")[x >= 0])
                })
            }
            else {
                for (i in seq(length(cols))) {
                    if (comvals[i] == 0) {
                        colnames(mat)[cols[i]] <- tolower(colnames(mat)[cols[i]])
                    }
                }
                cf <- colnames(mat)[cols]
                rowsf <- lapply(seq(nrow(temp)), function(x) {
                    x <- temp[x, ]
                    nms <- names(x)
                    nms[x == 0] <- tolower(nms[x == 0])
                    return(nms[x >= 0])
                })
            }
            trowsf <- table(unlist(rowsf))
            if (any(trowsf == length(rowsf))) {
                c2 <- names(trowsf)[trowsf == length(rowsf)]
                cf <- c(cf, c2[c2 != ""])
                rowsf <- lapply(rowsf, setdiff, c2)
            }
            rowsf1 <- rowsf[rowsf != ""]
            rowsf[rowsf != ""] <- rowsf1[order(match(toupper(gsub("[^A-Za-z]", "", rowsf1)), snames))]
            rowsf <- sapply(rowsf, paste, collapse = collapse)
            rowsf <- unique(setdiff(rowsf, ""))
            if (all(nchar(unique(rowsf)) == 1)) {
                tblchar <- table(toupper(rowsf))
                if (any(tblchar > 1)) {
                    for (ch in names(tblchar)[tblchar > 1]) {
                        rowsf <- rowsf[-which(toupper(rowsf) == ch)]
                    }
                }
            }
            rowsf <- paste(rowsf, collapse = " + ")
            cf <- paste(cf[order(match(toupper(gsub("[^A-Za-z]", "", cf)), snames))], collapse = collapse)
            pasted <- paste(cf, rowsf, sep="@")
        }
        else {
            if (mv) {
                pasted <- paste(sapply(seq(nrow(mat)), function(x) {
                    x <- mat[x, ]
                    paste(paste(names(x), "{", x, "}", sep = "")[x >= 0], collapse = collapse)
                }), collapse = " + ")
            }
            else if (use.tilde) {
                pasted <- paste(sapply(seq(nrow(mat)), function(x) {
                    colns <- colnames(mat)
                    colns[mat[x, ] == 0] <- paste("~", colns[mat[x, ] == 0], sep="")
                    paste(colns[mat[x, ] >= 0], collapse = collapse)
                }), collapse = " + ")
            }
            else {
                pasted <- paste(sapply(seq(nrow(mat)), function(x) {
                    colns <- colnames(mat)
                    colns[mat[x, ] == 0] <- tolower(colns[mat[x, ] == 0])
                    paste(colns[mat[x, ] >= 0], collapse = collapse)
                }), collapse = " + ")
            }
        }
        return(pasted)
    }
    getFacts <- function(mat) {
        cfound <- FALSE
        result <- list()
        for (cc in seq(ncol(mat))) {
            allcols <- combinations(ncol(mat), cc)
            for (cols in seq(ncol(allcols))) {
                temp <- mat[, allcols[, cols], drop = FALSE]
                uniq <- unique(temp)
                uniq <- uniq[apply(uniq, 1, function(x) all(x >= 0)), , drop = FALSE]
                if (nrow(uniq) > 0) {
                    for (i in seq(nrow(uniq))) {
                        rows <- logical(nrow(mat))
                        comrows <- apply(temp, 1, function(x) { all(x == unname(uniq[i, ])) })
                        if (sum(comrows) > 1) {
                            cfound <- TRUE
                            rows <- rows | comrows
                            pasted <- pasteit(mat, comrows, allcols[, cols], unname(uniq[i, ]))
                            if (sum(rows) < nrow(mat)) {
                                result[[length(result) + 1]] <- Recall(mat[!rows, , drop = FALSE])
                                names(result)[length(result)] <- pasted
                            }
                            else {
                                result <- list(NA)
                                names(result) <- pasted
                            }
                        }
                    }
                }
            }
        }
        if (!cfound) {
            result <- list(NA)
            names(result) <- pasteit(mat)
        }
        return(result)
    }
    getSol <- function(sol, collapse, pos) {
        pospos <- FALSE
        sol <- lapply(unique(lapply(sol, sort)), function(x) {
            x <- strsplit(x, split = "@")
            if (pos) {
                tbl <- table(unlist(x))
                if (any(tbl > 1)) {
                    tbl <- names(tbl)[tbl > 1]
                    checked <- logical(length(x))
                    common <- vector(mode = "list", length(tbl))
                    names(common) <- tbl
                    for (i in seq(length(tbl))) {
                        for (j in seq(length(x))) {
                            if (!checked[j]) {
                                if (any(x[[j]] == tbl[i])) {
                                    common[[i]] <- c(common[[i]], setdiff(x[[j]], tbl[i]))
                                    checked[j] <- TRUE
                                }
                            }
                        }
                        common[[i]] <- sort(common[[i]])
                    }
                    common <- paste(as.vector(sapply(seq(length(common)), function(x) {
                        sort(c(paste("(", paste(common[[x]], collapse = " + "), ")", sep = ""),
                               paste("(", paste(tbl[x], collapse = " + "), ")", sep="")))
                    })), collapse = collapse)
                    x <- x[!checked]
                    if (length(x) > 0) {
                        common <- paste(c(common, sapply(x[order(match(toupper(gsub("[^A-Za-z]", "", x)), snames))], paste, collapse = collapse)), collapse = " + ")
                    }
                    return(common)
                }
                else {
                    x <- sort(sapply(x, function(y) {
                        if (length(y) == 1) {
                            return(y)
                        }
                        paste(y[1], collapse, "(", y[2], ")", sep="")
                    }))
                }
            }
            else {
                x <- sort(sapply(x, function(y) {
                    if (length(y) == 1) {
                        return(y)
                    }
                    paste(y[1], collapse, "(", y[2], ")", sep="")
                }))
            }
            return(x)
        })
        sol <- unlist(lapply(unique(sol), function(x) {
            paste(x, collapse = " + ")
        }))
        return(sol)
    }
    isol <- NULL
    if (methods::is(input, "qca")) {
        collapse <- input$options$collapse
        noflevels <- input$tt$noflevels
        snames <- input$tt$options$conditions
        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
        }
        if (is.element("i.sol", names(input))) {
            elengths <- unlist(lapply(input$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(input$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            input <- unlist(lapply(input$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
        }
        else {
            input <- unlist(lapply(input$solution, paste, collapse = " + "))
        }
    }
    else if (methods::is(input, "deMorgan")) {
        if (any(attributes(input) == "snames")) {
            snames <- attr(input, "snames")
        }
    }
    factorizeit <- function(x, snames, noflevels) {
        x <- sop(x, snames = snames, noflevels = noflevels)
        trexp <- translate(x, snames = snames, noflevels = noflevels)
        snames <- colnames(trexp)
        getSol(lapply(
            names(unlist(getFacts(trexp))),
            function(x) {
                unlist(strsplit(x, split = "[.]"))
            }
        ), collapse = collapse, pos = pos)
    }
    if (is.character(input)) {
        if (!identical(snames, "")) {
            snames <- splitstr(snames)
        }
        collapse <- ifelse(any(grepl("[*]", input)), "*", "")
        mv <- any(grepl("[{]", unlist(input)))
        if (!use.tilde & any(hastilde(unlist(input)))) {
            use.tilde <- TRUE
        }
        result <- lapply(input, factorizeit, snames = snames, noflevels = noflevels)
        names(result) <- unname(input)
        if (!identical(snames, "")) {
            attr(result, "snames") <- snames
        }
        if (!is.null(isol)) {
            attr(result, "isol") <- isol
        }
        class(result) <- c("character", "factorize")
        return(result)
    }
}
