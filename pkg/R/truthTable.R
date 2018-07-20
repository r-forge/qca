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

`truthTable` <-
function(data, outcome = "", conditions = "", incl.cut = 1, n.cut = 1,
         complete = FALSE, use.letters = FALSE, show.cases = FALSE,
         dcc = FALSE, sort.by = "", inf.test = "", ...) {
    metacall <- match.call(expand.dots = TRUE)
    other.args <- list(...)
    back.args <- c("outcome", "conditions", "n.cut", "incl.cut", "complete", "show.cases", sort.by = "", "use.letters", "inf.test")
    check.args <- pmatch(names(other.args), back.args)
    names(other.args)[!is.na(check.args)] <- back.args[check.args[!is.na(check.args)]]
    enter       <- ifelse (is.element("enter",       names(other.args)), other.args$enter,       TRUE)
    ic0 <- 1
    if (is.character(incl.cut) & length(incl.cut) == 1) {
        incl.cut <- splitstr(incl.cut)
    }
    ic1 <- incl.cut[1]
    if (length(incl.cut) > 1) {
        ic0 <- incl.cut[2]
    }
        neg.out <- FALSE
        if ("neg.out" %in% names(other.args)) {
            neg.out <- other.args$neg.out
        }
        if ("incl.cut1" %in% names(other.args) & identical(ic1, 1)) {
            ic1 <- other.args$incl.cut1
            incl.cut[1] <- ic1
        }
        if ("incl.cut0" %in% names(other.args) & identical(ic0, 1)) {
            ic0 <- other.args$incl.cut0
            incl.cut[2] <- ic0
        }
    initialcols <- colnames(data)
    colnames(data) <- toupper(colnames(data))
    conditions <- toupper(conditions)
    outcome <- toupper(outcome)
    if (length(outcome) > 1) {
        cat("\n")
        stop(simpleError(paste0("Only one outcome is allowed.", ifelse(enter, "\n\n", ""))))
    }
    outcome.copy <- outcome
    initial.data <- as.data.frame(data) 
    if (tilde1st(outcome)) {
        neg.out <- TRUE
        outcome <- substring(outcome, 2)
    }
    if (!identical(outcome, "")) {
        if (! toupper(curlyBrackets(outcome, outside = TRUE)) %in% colnames(data)) {
            cat("\n")
            stop(simpleError(paste0("Inexisting outcome name.", ifelse(enter, "\n\n", ""))))
        }
    }
    if (grepl("[{|}]", outcome)) {
        outcome.value <- curlyBrackets(outcome)
        outcome <- curlyBrackets(outcome, outside=TRUE)
        data[, toupper(outcome)] <- as.numeric(data[, toupper(outcome)] %in% splitstr(outcome.value))
    }
    if (identical(conditions, "")) {
        conditions <- setdiff(colnames(data), outcome)
    }
    else {
        if (is.character(conditions) & length(conditions) == 1) {
            conditions <- splitstr(conditions)
        }
    }
    if (is.character(sort.by) & length(sort.by) == 1 & !identical(sort.by, "")) {
        sort.by <- splitstr(sort.by)
    }
    decreasing <- TRUE 
    if ("decreasing" %in% names(other.args)) {
        decreasing <- other.args$decreasing
    }
    if (is.character(decreasing) & length(decreasing) == 1) {
        decreasing <- splitstr(decreasing)
    }
    if (!identical(inf.test, "")) {
        inf.test <- splitstr(inf.test)
    }
    if (is.matrix(data)) {
        if (is.null(colnames(data))) {
            cat("\n")
            stop(simpleError(paste0("The data should have column names.", ifelse(enter, "\n\n", ""))))
        }
        if (any(duplicated(rownames(data)))) {
            rownames(data) <- seq(nrow(data))
        }
        data <- as.data.frame(data)
        for (i in seq(ncol(data))) {
            if (possibleNumeric(data[, i])) {
                data[, i] <- asNumeric(data[, i])
            }
        }
        initial.data <- data
    }
    verify.tt(data, outcome, conditions, complete, show.cases, ic1, ic0, inf.test)
    data <- data[, c(conditions, outcome)]
    colnames(data) <- toupper(colnames(data))
    conditions <- toupper(conditions)
    outcome <- toupper(outcome)
    if (neg.out) {
        data[, outcome] <- 1 - data[, outcome]
    }
    nofconditions <- length(conditions)
    infodata  <- getInfo(data, conditions, outcome)
    data      <- infodata$data 
    hastime   <- infodata$hastime
    fuzzy.cc  <- infodata$fuzzy.cc
    noflevels <- infodata$noflevels
    dc.code   <- infodata$dc.code
    rownames(data) <- rownames(initial.data)
    condata <- data[, conditions, drop = FALSE]
    if (any(fuzzy.cc)) {
        if (any(data[, conditions[fuzzy.cc]] == 0.5)) {
            cat("\n")
            warning("Fuzzy causal conditions should not have values of 0.5 in the data.\n")
        }
        condata[, fuzzy.cc] <- lapply(condata[, fuzzy.cc, drop = FALSE], function(x) as.numeric(x > 0.5))
    }
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    line.data <- as.vector(as.matrix(condata) %*% mbase) + 1
    condata <- condata[order(line.data), , drop = FALSE]
    uniq <- which(!duplicated(condata))
    tt <- condata[uniq, , drop = FALSE]
    rownstt <- sort(line.data)[uniq]
    rownames(tt) <- rownstt
    ipc <- .Call("truthTable", as.matrix(data[, conditions]), data[, outcome], as.matrix(tt), as.numeric(fuzzy.cc), PACKAGE = "QCA")
    colnames(ipc) <- rownstt
    minmat <- ipc[seq(4, nrow(ipc)), , drop = FALSE]
    ipc <- ipc[1:3, , drop = FALSE]
    rownames(minmat) <- rownames(data)
    rownames(ipc) <- c("n", "incl", "PRI")
    exclude <- ipc[1, ] < n.cut
    if (sum(!exclude) == 0) {
        cat("\n")
        stop(simpleError(paste0("There are no combinations at this frequency cutoff.", ifelse(enter, "\n\n", ""))))
    }
    tt$OUT <- "?"
    tt$OUT[!exclude] <- as.numeric(ipc[2, !exclude] >= (ic1 - .Machine$double.eps ^ 0.5))
    tt$OUT[ipc[2, !exclude] <= (ic1 - .Machine$double.eps ^ 0.5) & ipc[2, !exclude] >= (ic0 - .Machine$double.eps ^ 0.5)] <- "C"
    tt <- cbind(tt, t(ipc))
    cases <- sapply(rownstt, function(x) {
        paste(rownames(data)[line.data == x], collapse = ",")
    })
    DCC <- apply(minmat, 2, function(x) {
        paste(rownames(data)[x > 0.5 & data[, outcome] < 0.5], collapse = ",")
    })
    casesexcl <- cases[exclude]
    rownstt <- rownstt[!exclude]
    cases <- cases[!exclude]
    DCC <- DCC[!exclude]
    excluded <- tt[exclude, , drop = FALSE]
    excluded$OUT <- as.numeric(ipc[2, exclude] >= (ic1 - .Machine$double.eps ^ 0.5))
    excluded$OUT[ipc[2, exclude] < ic1 & ipc[2, exclude] >= (ic0 - .Machine$double.eps ^ 0.5)]  <- "C"
    if (length(conditions) < 8) {
        ttc <- as.data.frame(matrix(nrow = prod(noflevels), ncol = ncol(tt)))
        colnames(ttc) <- colnames(tt)
        ttc[, seq(length(conditions))] <- createMatrix(noflevels)
        ttc$OUT   <- "?"
        ttc$n     <-  0
        ttc$incl  <- "-"
        whichpri <- which(colnames(ttc) == "PRI")
        ttc[, whichpri[length(whichpri)]] <- "-"  
        ttc[rownames(tt), ] <- tt
        tt <- ttc
    }
    else {
        tt <- tt[!exclude, , drop = FALSE]
    }
    if (!identical(sort.by, "")) {
        if (is.logical(sort.by)) { 
            decreasing <- as.vector(sort.by)
            sort.by <- names(sort.by)
        }
        else {
            if (missing(decreasing)) {
                decreasing <- rep(TRUE, length(sort.by))
            }
            else {
                if (is.logical(decreasing)) {
                    if (length(decreasing) == 1) {
                        decreasing <- rep(decreasing, length(sort.by))
                    }
                    else if (length(decreasing) < length(sort.by)) {
                        decreasing <- c(decreasing, rep(TRUE, length(sort.by) - length(decreasing)))
                    }
                }
                else {
                    decreasing <- rep(TRUE, length(sort.by))
                }
            }
        }
        sort.by[sort.by == "out"] <- "OUT"
        decreasing <- decreasing[is.element(sort.by, names(tt))]
        sort.by <- sort.by[is.element(sort.by, names(tt))]
        rowsorder <- seq_len(nrow(tt))
        for (i in rev(seq(length(sort.by)))) {
            rowsorder <- rowsorder[order(tt[rowsorder, sort.by[i]], decreasing = decreasing[i])]
        }
        sortvector <- rep(1, nrow(tt))
        sortvector[tt[rowsorder, "OUT"] == "?"] <- 2
        rowsorder <- rowsorder[order(sortvector)]
    }
    uppercols <- toupper(colnames(initial.data))
    for (i in seq(length(conditions))) {
        if (hastime[i]) {
            tt[, i][tt[, i] == max(tt[, i])] <- dc.code
            data[, i][data[, i] == max(data[, i])] <- -1 
            noflevels[i] <- noflevels[i] - 1
        }
    }
    statistical.testing <- FALSE
    if (inf.test[1] == "binom") {
        statistical.testing <- TRUE
        if (length(inf.test) > 1) {
            alpha <- as.numeric(inf.test[2]) 
        }
        else {
            alpha <- 0.05
        }
        observed <- which(tt$OUT != "?")
        success <- round(tt[observed, "n"] * as.numeric(tt[observed, "incl"]))
        tt$pval1 <- "-"
        if (length(incl.cut) > 1) {
            tt$pval0 <- "-"
        }
        tt[observed, "OUT"] <- 0
        for (i in seq(length(observed))) {
            pval1 <- tt[observed[i], "pval1"] <- binom.test(success[i], tt[observed[i], "n"], p = ic1, alternative = "greater")$p.value
            if (length(incl.cut) > 1) {
                pval0 <- tt[observed[i], "pval0"] <- binom.test(success[i], tt[observed[i], "n"], p = ic0, alternative = "greater")$p.value
            }
            if (pval1 < alpha) {
                tt[observed[i], "OUT"] <- 1
            }
            else if (length(incl.cut) > 1) {
                if (pval0 < alpha) {
                    tt[observed[i], "OUT"] <- "C"
                }
            }
        }
    }
        tt$cases <- ""
        if (length(conditions) < 8) {
            tt$cases[rownstt] <- cases
        }
        else {
            tt$cases <- cases
        }
    numerics <- unlist(lapply(initial.data, possibleNumeric))
    colnames(initial.data)[!numerics] <- initialcols[!numerics]
    x <- list(tt = tt, indexes = rownstt, noflevels = as.vector(noflevels),
              initial.data = initial.data, recoded.data = data, cases = cases, DCC = DCC, minmat = minmat,
              options = list(outcome = outcome.copy, conditions = conditions, neg.out = neg.out, n.cut = n.cut,
                             incl.cut = incl.cut, complete = complete, show.cases = show.cases, dcc = dcc,
                             use.letters = use.letters, inf.test = statistical.testing))
    if (any(exclude)) {
        excluded$cases <- ""
        excluded$cases <- casesexcl
        x$excluded <- structure(list(tt = excluded,
                                     options = list(complete = FALSE, show.cases = show.cases, dcc = dcc, excluded = TRUE)), class="tt")
    }
    if (use.letters & any(nchar(conditions) > 1)) { 
        colnames(x$tt)[seq(nofconditions)] <- LETTERS[seq(nofconditions)]
    }
    if (!identical(sort.by, "")) {
        x$rowsorder <- rowsorder
    }
    x$fs <- unname(fuzzy.cc)
    x$call <- metacall
    return(structure(x, class="tt"))
}
