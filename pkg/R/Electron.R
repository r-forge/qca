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

`GUIcall` <- function(commandlist) {
    ev <- get("invisibleEnvironment", envir = globalenv())
    nms <- names(commandlist)
    result <- c()
    `hashobjs` <- function(...) {
        return(lapply(globalenv(), function(x) fastdigest::fastdigest(x)))
    }
    `jsonify` <- function(x) {
        nms <- names(x)
        result <- ""
        for (i in seq(length(x))) {
            xi <- x[[i]]
            if (inherits(xi, "list")) {
                if (length(xi) > 0) {
                    nmsi <- names(xi)
                    if (is.null(nmsi)) {
                        result <- paste(result, "'", nms[i], "'", ": [", Recall(xi), "]",  sep = "")
                    }
                    else {
                        if (is.null(xi)) {
                            result <- paste(result, "'", nms[i], "'", ": undefined", sep = "")
                        }
                        else {
                            result <- paste(result, "'", nms[i], "'", ": {", Recall(xi), "}",  sep = "")
                        }
                    }
                }
                else {
                    result <- paste(result, "'", nms[i], "'", ": {}",  sep = "")
                }
            }
            else {
                collapse <- ", "
                prefix <- ""
                if (is.character(xi)) {
                    collapse <- "`, `"
                    prefix <- "`"
                }
                if (is.logical(x[[i]])) x[[i]] <- QCA::recode(x[[i]], "TRUE = true; FALSE = false")
                result <- paste(result,
                    ifelse (is.null(nms[i]), 
                        sprintf(ifelse(length(x[[i]]) > 1, " [ %s%s%s ]", "%s%s%s"), prefix, paste(x[[i]], collapse = collapse), prefix),
                        sprintf(ifelse(length(x[[i]]) > 1, "'%s': [ %s%s%s ]", "'%s': %s%s%s"), nms[i], prefix, paste(x[[i]], collapse = collapse), prefix)
                    )
                )
            }
            if (i < length(x)) {
                result <- paste(result, ",", sep = "")
            }
        }
        return(result)
    }
    `scrollobj` <- function(...) {
        x <- list(...)
        scrollvh <- lapply(x$scrollvh, function(x) unlist(x) + 1)
        ev$visiblerows <- x$visiblerows + 1
        ev$visiblecols <- x$visiblecols + 1
        if (!x$alldata) {
            scrollvh <- scrollvh[x$dataset]
        }
        tosend <- vector(mode = "list", length = length(scrollvh))
        names(tosend) <- names(scrollvh)
        for (n in names(scrollvh)) {
            data <- get(n, ev)
            nrowd <- nrow(data)
            ncold <- ncol(data)
            dscrollvh <- scrollvh[[n]]
            srow <- min(dscrollvh[1], nrowd - min(nrowd, x$visiblerows) + 1)
            scol <- min(dscrollvh[2], ncold - min(ncold, x$visiblecols) + 1)
            erow <- min(srow + x$visiblerows, nrowd)
            ecol <- min(scol + x$visiblecols, ncold)
            tosend[[n]] <- list(
                theData = unname(as.list(data[seq(srow, erow), seq(scol, ecol), drop = FALSE])),
                dataCoords = paste(srow, scol, erow, ecol, ncold, sep="_"),
                scrollvh = c(srow, scol) - 1
            )
        }
        return(jsonify(list(scrollData = tosend)))
    }
    `infobjs` <- function(objs, scrollvh) {
        funargs <- lapply(match.call(), deparse)
        type <- funargs$objs
        if (!identical(type, "added") & !identical(type, "modified")) {
            type <- "infobjs"
        }
        visiblerows <- ev$visiblerows
        visiblecols <- ev$visiblecols
        misscroll <- missing(scrollvh)
        toreturn <- list()
        objtype <- unlist(lapply(mget(objs, globalenv()), function(x) {
            if (is.data.frame(x)) {
                return(1)
            }
            else if (is(x, "tt")) {
                return(2)
            }
            else if (is(x, "qca")) {
                return(3)
            }
            return(0)
        }))
        if (any(objtype > 0)) {
            if (any(objtype == 1)) { 
                toreturn$data <- lapply(names(objtype[objtype == 1]), function(n) {
                    x <- globalenv()[[n]]
                    dscrollvh <- c(1, 1)
                    if (!misscroll) {
                        if (is.element(n, names(scrollvh))) {
                            dscrollvh <- scrollvh[[n]]
                        }
                    }
                    nrowd <- nrow(x)
                    ncold <- ncol(x)
                    srow <- min(dscrollvh[1], nrowd - min(nrowd, visiblerows) + 1)
                    scol <- min(dscrollvh[2], ncold - min(ncold, visiblecols) + 1)
                    erow <- min(srow + visiblerows - 1, nrowd)
                    ecol <- min(scol + visiblecols - 1, ncold)
                    return(list(
                        nrows = nrowd,
                        ncols = ncold,
                        rownames = rownames(x),
                        colnames = colnames(x),
                        numerics = as.vector(unlist(lapply(x, QCA::possibleNumeric))),
                        calibrated = as.vector(unlist(lapply(x, function(x) {
                            all(na.omit(x) >= 0 & na.omit(x) <= 1)
                        }))),
                        binary = as.vector(unlist(lapply(x, function(x) all(is.element(x, 0:1))))),
                        scrollvh = c(srow, scol) - 1, 
                        theData = unname(as.list(x[seq(srow, erow), seq(scol, ecol), drop = FALSE])),
                        dataCoords = paste(srow, scol, erow, ecol, ncol(x), sep = "_")
                    ))
                })
                names(toreturn$data) <- names(objtype[objtype == 1])
            }
            if (any(objtype == 2)) { 
                toreturn$tt <- lapply(mget(names(objtype[objtype == 2]), globalenv()), function(x) {
                    components <- c("indexes", "noflevels", "cases", "options", "colnames", "numerics")
                    x$indexes <- x$indexes - 1 
                    x$options$conditions <- toupper(x$options$conditions)
                    cnds <- x$options$conditions
                    if (x$options$use.letters) {
                        cnds <- LETTERS[seq(length(cnds))]
                    }
                    x$options$outcome <- list(notilde(x$options$outcome))
                    if (length(x$options$incl.cut) == 1) {
                        x$options$incl.cut <- list(x$options$incl.cut)
                    }
                    if (length(cnds) <= 7) {
                        x$id <- apply(x$tt[, cnds], 1, function(x) {
                            ifelse(any(x == 1), paste(which(x == 1), collapse=""), "0")
                        })
                        components <- c(components, "id", "tt")
                    }
                    x$colnames <- colnames(x$initial.data)
                    x$numerics <- as.vector(unlist(lapply(x$initial.data, QCA::possibleNumeric)))
                    return(x[components])
                })
            }
            if (any(objtype == 3)) { 
                toreturn$qmc <- lapply(mget(names(objtype[objtype == 3]), .GlobalEnv), function(x) {
                    components <- c("indexes", "noflevels", "cases", "options")
                    x <- x$tt
                    x$options$conditions <- toupper(x$options$conditions)
                    cnds <- x$options$conditions
                    if (x$options$use.letters) {
                        cnds <- LETTERS[seq(length(cnds))]
                    }
                    if (length(cnds) <= 7) {
                        x$id <- apply(x$tt[, cnds], 1, function(x) {
                            ifelse(any(x == 1), paste(which(x == 1), collapse=""), "0")
                        })
                        components <- c(components, "id", "tt")
                    }
                    x$indexes <- x$indexes - 1 
                    return(x[components])
                })
            }
            toreturn <- list(toreturn)
            names(toreturn) <- type
            return(jsonify(toreturn))
        }
    }
    `Changes` <- function(...) {
        changes <- gsub("`", "'", readLines(system.file("ChangeLog", package = "QCA")))
        return(jsonify(list(changes = changes)))
    }
    `packages` <- function(x) { 
        attached <- data()$results[, -2]
        packages <- unique(attached[, "Package"])
        if (!identical(sort(packages), sort(x))) {
            attached <- lapply(packages, function(x) {
                x <- attached[attached[, "Package"] == x, 2:3, drop = FALSE]
                x <- x[x[, 2] != "Internal Functions", , drop = FALSE] 
                if (nrow(x) == 0) return(list())
                titles <- as.list(x[, 2])
                names(titles) <- x[, 1]
                return(titles) 
            })
            names(attached) <- packages
            return(jsonify(list(packages = attached)))
        }
    }
    `xyplot` <- function(...) {
        arglist <- list(...)
        if (is.element("dataset", names(arglist))) {
            xyplot_before <- fastdigest::fastdigest(get(arglist$dataset, globalenv())[, c(arglist$x, arglist$y), drop = FALSE])
        }
    }
    `calibration` <- function(...) {
        arglist <- list(...)
        if (is.element("dataset", names(arglist))) {
            hashcalib <- fastdigest::fastdigest(get(arglist$dataset, globalenv())[, arglist$x, drop = FALSE])
            if (arglist$thsetter) {
                recalibrate <- TRUE
            }
        }
    }
    `thinfo` <- function(...) {
    }
    `scrollvh` <- function(...) {
    }
    `editorSize` <- function(visiblerows, visiblecols) {
        ev$visiblerows <- visiblerows
        ev$visiblecols <- visiblecols
    }
    for (n in nms) {
        if (is.element(n, c("source", "options", "library"))) {
            do.call(n, commandlist[[n]])
        }
        else {
            result <- c(result, do.call(n, commandlist[[n]]))
        }
    }
    hashes <- hashobjs()
    added <- setdiff(names(hashes), names(ev$hashes))
    deleted <- setdiff(names(ev$hashes), names(hashes))
    common <- intersect(names(hashes), names(ev$hashes))
    modified <- names(ev$hashes)[!is.element(ev$hashes[common], hashes[common])]
    ev$hashes <- hashes
    if (length(added) > 0) result <- c(result, infobjs(added))
    if (length(modified) > 0) result <- c(result, infobjs(modified))
    if (length(deleted) > 0) result <- c(result, jsonify(list(deleted = deleted)))
    savehistory(file = "temphistory")
    history <- readLines("temphistory")
    if (ev$firstHistory) {
        ev$firstHistory <- FALSE
        history[length(history) - 1] <- "library(QCA)"
    }
    writeLines(history[seq(length(history) - 1)], con = "temphistory")
    loadhistory(file = "temphistory")
    unlink(".temphistory")
    if (length(result) > 0) {
        cat("{", paste(result, collapse = ", "), "}")
    }
}
