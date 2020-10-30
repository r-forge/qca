# Copyright (c) 2020, Adrian Dusa
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

`pof` <-
function(setms = NULL, outcome = NULL, data = NULL, relation = "necessity",
         inf.test = "", incl.cut = c(0.75, 0.5), add = NULL, ...) {
    setms <- admisc::recreate(substitute(setms))
    outcome <- admisc::recreate(outcome)
    funargs <- lapply(lapply(match.call(), deparse)[-1], function(x) gsub("\"|[[:space:]]", "", x))
    other.args <- list(...)
    funargs$outcome <- paste(funargs$outcome, collapse = "")
    if (is.null(setms)) {
        cat("\n")
        stop(simpleError("The \"setms\" argument is missing.\n\n"))
    }
    if (!(nec(relation) | suf(relation))) {
        cat("\n")
        stop(simpleError("The relation should be either \"necessity\" or \"sufficiency\".\n\n"))
    }
    icp <- 0.75
    ica <- 0.5
    if (is.character(incl.cut) & length(incl.cut) == 1) {
        incl.cut <- admisc::splitstr(incl.cut)
    }
    icp <- incl.cut[1]
    if (length(incl.cut) > 1) {
        ica <- incl.cut[2]
    }
        neg.out <- FALSE
        if (is.element("neg.out", names(other.args))) {
            neg.out <- other.args$neg.out
        }
        if (is.element("incl.cut1", names(other.args)) & identical(icp, 0.75)) {
            icp <- other.args$incl.cut1
        }
        if (is.element("incl.cut0", names(other.args)) & identical(ica, 0.5)) {
            ica <- other.args$incl.cut0
        }
    complete <- FALSE
    if (is.element("complete", names(other.args))) {
        if (is.logical(other.args$complete)) {
            complete <- other.args$complete
        }
    }
    if (!is.null(data)) {
        if (is.element("data.frame", class(data)) | is.matrix(data)) {
            data <- as.data.frame(data)
        }
        for (i in colnames(data)) {
            if (!is.numeric(data[, i])) {
                if (admisc::possibleNumeric(data[, i])) {
                    data[, i] <- admisc::asNumeric(data[, i])
                }
            }
        }
        if (is.element("minimize", names(other.args))) {
            if (is.element("use.letters", names(other.args))) {
                if (other.args$use.letters) {
                    colnames(data)[seq(1, ncol(data) - 1)] <- LETTERS[seq(1, ncol(data) - 1)]
                }
            }
        }
    }
    conditions <- outcomename <- ""
    condnegated <- outnegated <- FALSE
    `extract` <- function(x, snames = "", data = NULL) {
        if (grepl("<=>|<->", x)) {
            cat("\n")
            stop(simpleError("Incorrect expression: relation can be either necessity or sufficiency.\n\n"))
        }
        multivalue <- grepl("\\{|\\}|\\[|\\]", x)
        relation <- ifelse(grepl("=|-", x), ifelse(grepl("=>|->", x), "suf", "nec"), NA)
        x <- gsub("<=|=>|<-|->", "@", gsub("[[:space:]]", "", x))
        x <- unlist(strsplit(x, split = "@"))
        if (identical(snames, "") & !is.null(data)) {
            snames <- colnames(data)
        }
        if (identical(substring(x[1], 1, 2), "1-")) {
            x[1] <- negate(gsub("1-", "", x[1]), snames = snames)
        }
        if (identical(substring(x[2], 1, 2), "1-")) {
            x[2] <- negate(gsub("1-", "", x[2]), snames = snames)
        }
        outmtrx <- NA
        if (length(x) > 1) {
            outmtrx <- admisc::validateNames(x[2], snames = snames, data = data)
        }
        if (!is.na(outmtrx) & !is.null(data)) {
            data <- data[, -which(is.element(colnames(data), colnames(outmtrx)))]
        }
        condmtrx <- admisc::validateNames(x[1], snames = snames, data = data)
        return(list(condmtrx = condmtrx, outmtrx = outmtrx, expression = x[1],
            relation = relation, multivalue = multivalue))
    }
    checkoutcome <- TRUE
    addexpression <- FALSE
    if (is.element("character", class(setms))) {
        if (missing(data)) {
            cat("\n")
            stop(simpleError("The data argument is missing, with no default.\n\n"))
        }
        if (length(setms) > 1) {
            cat("\n")
            stop(simpleError("Only one expression allowed.\n\n"))
        }
        toverify <- extract(setms, data = data)
        if (!is.na(toverify$relation)) {
            relation <- toverify$relation
        }
        conditions <- colnames(toverify$condmtrx)
        if (is.na(toverify$outmtrx)) {
            if (missing(outcome)) {
                cat("\n")
                stop(simpleError("Expression without outcome.\n\n"))
            }
            temp <- subset(data, select = which(is.element(colnames(data), conditions)))
            verify.qca(temp)
            setms <- admisc::compute(toverify$expression, data = temp, separate = TRUE)
            funargs$setms <- toverify$expression
        }
        else {
            outcomename <- colnames(toverify$outmtrx)[1]
            temp <- subset(data, select = which(is.element(colnames(data), c(conditions, outcomename))))
            verify.qca(temp)
            setms <- admisc::compute(toverify$expression, data = temp, separate = TRUE)
            funargs$setms <- paste(paste(unlist(toverify$expression), collapse = "+"),
                                    ifelse(toverify$relation == "suf", "->", "<-"),
                                    rownames(toverify$outmtrx))
            outcome <- admisc::compute(rownames(toverify$outmtrx)[1], data = temp) 
            checkoutcome <- FALSE
        }
        if (is.vector(setms)) {
            setms <- data.frame(setms)
            colnames(setms) <- toverify$expression
        }
        rownames(setms) <- rownames(data)
        if (!is.element("minimize", names(other.args)) & ncol(setms) > 1) {
            addexpression <- TRUE
        }
    }
    if (is.element("QCA_fuzzy", class(setms))) {
        conditions <- "expression"
        setms <- data.frame(X = as.vector(setms))
        colnames(setms) <- conditions
    }
    if (checkoutcome) {
        if (missing(outcome)) {
            cat("\n")
            stop(simpleError("Outcome is missing, with no default.\n\n"))
        }
        if (is.element("character", class(outcome))) {
            if (admisc::tilde1st(gsub("1-", "", funargs$outcome))) {
                outnegated <- !outnegated
            }
            if (identical(substr(funargs$outcome, 1, 2), "1-")) {
                outnegated <- !outnegated
            }
            outcome <- admisc::notilde(gsub("1-", "", funargs$outcome))
            if (grepl("\\{", outcome)) {
                outcomename <- admisc::curlyBrackets(outcome, outside = TRUE)
            }
            else {
                outcomename <- admisc::squareBrackets(outcome, outside = TRUE)
            }
            if (is.null(data)) {
                cat("\n")
                stop(simpleError("The data argument is missing, with no default.\n\n"))
            }
            if (!is.element(outcomename, colnames(data))) {
                cat("\n")
                stop(simpleError("Outcome not found in the data.\n\n"))
            }
            verify.qca(data[, which(colnames(data) == outcomename), drop = FALSE])
            outcome <- admisc::compute(outcome, data = data)
            if (outnegated) {
                outcome <- 1 - outcome
            }
        }
        else if (is.vector(outcome)) {
            if (admisc::tilde1st(gsub("1-", "", funargs$outcome))) {
                outnegated <- !outnegated
            }
            if (identical(substr(funargs$outcome, 1, 2), "1-")) {
                outnegated <- !outnegated
            }
            outcomename <- admisc::notilde(gsub("1-", "", funargs$outcome))
            if (identical(substr(outcomename, 1, 2), "c(")) {
                outcomename <- "Y"
            }
        }
    }
    if (is.vector(outcome)) {
        if (!is.numeric(outcome) & admisc::possibleNumeric(outcome)) {
            outcome <- admisc::asNumeric(outcome)
        }
        verify.qca(outcome)
    }
    else {
        cat("\n")
        stop(simpleError("The outcome should be either a column name in a dataset\n       or a vector of set membership values.\n\n"))
    }
    if (identical(substr(funargs$setms, 1, 2), "1-")) {
        condnegated <- !condnegated
    }
    if (is.vector(setms)) {
        setms <- data.frame(setms)
        conditions <- admisc::notilde(gsub("1-", "", funargs$setms))
        if (grepl("[$]", conditions)) {
            conditions <- tail(unlist(strsplit(conditions, split = "[$]")), 1)
        }
        else if (identical(substr(conditions, 1, 2), "c(")) {
            conditions <- "X"
        }
        colnames(setms) <- conditions
    }
    if (is.element("data.frame", class(setms))) {
        for (i in seq(ncol(setms))) {
            if (!is.numeric(setms[, i]) & admisc::possibleNumeric(setms[, i])) {
                setms[, i] <- admisc::asNumeric(setms[, i])
            }
        }
        verify.qca(setms)
        colnames(setms) <- gsub("[[:space:]]", "", colnames(setms))
        if (identical(conditions, "")) {
            if (any(grepl("[*]", colnames(setms)))) {
                conditions <- colnames(extract(paste(colnames(setms), collapse = "+"))$condmtrx)
            }
        }
        if (condnegated) {
            if (any(grepl("[*]", conditions))) {
                conditions <- colnames(extract(paste(conditions, collapse = "+"))$condmtrx)
            }
            else if (any(grepl("[+]", conditions))) {
                conditions <- unique(unlist(strsplit(conditions, split = "[+]")))
            }
            if (any(grepl("\\$coms|\\$pims", funargs$setms))) {
                toverify <- unlist(strsplit(admisc::notilde(gsub("1-", "", funargs$setms)), split = "\\$"))[1]
                if (grepl("pims", funargs$setms)) { 
                    tt <- eval.parent(parse(text = sprintf("%s$tt", toverify)), n = 1)
                    if (tt$options$use.letters) {
                        conditions <- LETTERS[seq(length(conditions))]    
                    }
                    else {
                        conditions <- tt$options$conditions
                    }
                }
                else {
                    conditions <- eval.parent(parse(text = sprintf("%s$options$conditions", toverify)), n = 1)
                }
            }
            if (identical(conditions, "")) {
                colnames(setms) <- paste("~", colnames(setms), sep = "")
            }
            else {
                colnames(setms) <- gsub("[[:space:]]", "", admisc::negate(colnames(setms), snames = conditions))
            }
        }
    }
    else {
        cat("\n")
        stop(simpleError("The \"setms\" argument is not standard.\n\n"))
    }
    if (any(na.omit(cbind(setms, outcome) > 1))) {
        cat("\n")
        stop(simpleError("Set membership scores should be numbers between 0 and 1.\n\n"))
    }
    notmiss <- apply(cbind(setms, outcome), 1, function(x) !any(is.na(x)))
    outcome <- outcome[notmiss]
    setms <- setms[notmiss, , drop = FALSE]
    if (neg.out) {
        outcome <- admisc::getLevels(outcome) - outcome - 1
    }
    result.list <- list()
    incl.cov <- .Call("C_pof", as.matrix(cbind(setms, fuzzyor(setms))), outcome, nec(relation), PACKAGE = "QCA")
    incl.cov[incl.cov < 0.00001] <- 0 
    incl.cov <- as.data.frame(incl.cov)
    if (nec(relation)) {
        colnames(incl.cov) <- c("inclN", "RoN", "covN", "covU")
    }
    else {
        colnames(incl.cov) <- c("inclS", "PRI", "covS", "covU")
    }
    if (is.character(inf.test) & length(inf.test) == 1) {
        inf.test <- admisc::splitstr(inf.test)
    }
    if (!identical(inf.test, "")) {
        if (missing(data)) {
            data <- cbind(setms, outcome)
            colnames(data) <- c(conditions, outcomename)
        }
        verify.inf.test(inf.test, data)
    }
    if (identical(inf.test[1], "binom")) {
        statistical.testing <- TRUE
        if (length(inf.test) > 1) {
            alpha <- as.numeric(inf.test[2]) 
        }
        else {
            alpha <- 0.05
        }
        if (nec(relation)) {
            nofcases <- rep(sum(outcome), ncol(setms) + 1)
        }
        else {
            nofcases <- c(colSums(setms), sum(fuzzyor(setms)))
        }
        success <- as.vector(round(nofcases * incl.cov[, which(grepl("incl", colnames(incl.cov)))[1]]))
        incl.cov$pval0 <- incl.cov$pval1 <- 0
        for (i in seq(length(success))) {
            incl.cov[i, "pval1"] <- binom.test(success[i], nofcases[i], p = icp, alternative = "greater")$p.value
            incl.cov[i, "pval0"] <- binom.test(success[i], nofcases[i], p = ica, alternative = "greater")$p.value
        }
    }
    result.list$incl.cov <- incl.cov
    if (nec(relation)) {
        result.list$incl.cov <- result.list$incl.cov[, -4]
    }
    else {
        result.list$incl.cov[nrow(incl.cov), 4] <- NA
    }
    colnms <- colnames(setms)
    if (addexpression) {
        colnms <- c(colnms, "expression")
    }
    else {
        result.list$incl.cov <- result.list$incl.cov[-nrow(incl.cov), , drop = FALSE]
        if (nrow(result.list$incl.cov) == 1 & suf(relation)) {
            result.list$incl.cov[1, 4] <- NA
        }
    }
    rownames(result.list$incl.cov) <- colnms
    if (is.element("show.cases", names(other.args))) {
        if (other.args$show.cases) {
            result.list$incl.cov <- cbind(result.list$incl.cov, cases = other.args$cases, stringsAsFactors = FALSE)
        }
    }
    if (is.element("minimize", names(other.args))) {
        result.list$pims <- as.data.frame(setms)
        result.list$sol.incl.cov <- incl.cov[nrow(incl.cov), 1:3]
    }
    if (is.element("solution.list", names(other.args))) {
        solution.list <- other.args$solution.list
        length.solution <- length(solution.list)
        individual <- vector("list", length = length.solution)
        for (i in seq(length.solution)) {
            individual[[i]] <- list()
            temp <- setms[, solution.list[[i]], drop = FALSE]
            incl.cov <- .Call("C_pof", as.matrix(cbind(temp, fuzzyor(temp))), outcome, nec(relation), PACKAGE = "QCA")
            incl.cov[incl.cov < 0.0001] <- 0
            incl.cov <- as.data.frame(incl.cov)
            rownames(incl.cov) <- c(colnames(temp), "expression")
            if (nec(relation)) {
                colnames(incl.cov) <- c("inclN", "RoN", "covN", "covU")
                incl.cov <- incl.cov[, -4]
            }
            else {
                colnames(incl.cov) <- c("inclS", "PRI", "covS", "covU")
                incl.cov[nrow(incl.cov), 4] <- NA
            }
            if (nrow(incl.cov) == 2 & suf(relation)) {
                incl.cov[1, 4] <- NA
            }
            individual[[i]]$incl.cov <- incl.cov[-nrow(incl.cov), ]
            individual[[i]]$sol.incl.cov <- incl.cov[nrow(incl.cov), 1:3]
            individual[[i]]$pims <- as.data.frame(temp)
        }
        return(structure(list(overall=result.list, individual=individual, essential=other.args$essential, pims=as.data.frame(setms), relation=relation, options=funargs[-1]), class="QCA_pof"))
    }
    if (!is.null(add)) {
        if (!(is.list(add) | is.function(add))) {
            cat("\n")
            stop(simpleError("The argument \"add\" should be a function or a list of functions.\n\n"))
        }
        if (is.list(add)) {
            if (!all(unlist(lapply(add, is.function)))) {
                cat("\n")
                stop(simpleError("Components from the list argument \"add\" should be functions.\n\n"))
            }
            toadd <- matrix(nrow = nrow(incl.cov), ncol = length(add))
            if (is.null(names(add))) {
                names(add) <- paste0("X", seq(length(add)))
            }
            if (any(duplicated(substr(names(add), 1, 5)))) {
                names(add) <- paste0("X", seq(length(add)))
            }
            colnames(toadd) <- substr(names(add), 1, 5)
            for (i in seq(length(add))) {
                coltoadd <- apply(cbind(setms, fuzzyor(setms)), 2, add[[i]], outcome)
                if (ncol(setms) == 1) {
                    coltoadd <- coltoadd[1]
                }
                toadd[, i] <- coltoadd
            }
        }
        else {
            toadd <- matrix(nrow = nrow(incl.cov), ncol = 1)
            coltoadd <- apply(cbind(setms, fuzzyor(setms)), 2, add, outcome)
            if (ncol(setms) == 1) {
                coltoadd <- coltoadd[1]
            }
            toadd[, 1] <- coltoadd
            if (any(grepl("function", funargs$add))) {
                funargs$add <- "X"
            }
            colnames(toadd) <- substr(funargs$add, 1, 5)
        }
        result.list$incl.cov <- cbind(result.list$incl.cov, toadd)
    }
    funargs[["setms"]] <- setms
    funargs[["outcome"]] <- outcome
    funargs$relation <- relation
    result.list$options <- funargs
    return(structure(result.list, class = "QCA_pof"))
}
