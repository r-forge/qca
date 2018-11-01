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

`verify.data` <-
function(data, outcome = "", conditions = "") {
    if (!is.data.frame(data)) {
        cat("\n")
        stop(simpleError("The input data should be a data frame.\n\n"))
    }
    if (is.null(colnames(data))) {
        cat("\n")
        stop(simpleError("Please specify the column names for your data.\n\n"))
    }
    if (identical(outcome, "")) {
        cat("\n")
        stop(simpleError("The outcome set is not specified.\n\n"))
    }
    if (! outcome %in% colnames(data)) {
        cat("\n")
        stop(simpleError("The name of the outcome is not correct.\n\n"))
    }
    if (!identical(conditions, "")) {
        if (outcome %in% conditions) {
            cat("\n")
            stop(simpleError(paste0("Variable \"", outcome, "\" cannot be both outcome _and_ condition!\n\n")))
        }
        if (!all(conditions %in% names(data))) {
            cat("\n")
            stop(simpleError("The conditions' names are not correct.\n\n"))
        }
        if (any(duplicated(conditions))) {
            cat("\n")
            stop(simpleError("Duplicated conditions.\n\n"))
        }
    }
    if (any(is.na(data))) {
        checked <- sapply(data, function(x) any(is.na(x)))
        cat("\n")
        stop(paste("Missing values in the data are not allowed. Please check columns:\n",
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep=""), call. = FALSE)
    }
}
`verify.qca` <-
function(data) {
    if (is.data.frame(data)) {
        if (is.null(colnames(data))) {
            cat("\n")
            stop(simpleError("The dataset doesn't have any columns names.\n\n"))
        }
        checkNumUncal <- lapply(data, function(x) {
            x <- setdiff(x, c("-", "dc", "?"))
            pn <- possibleNumeric(x)
            uncal <- mvuncal <- FALSE
            if (pn) {
                y <- asNumeric(x)
                if (any(y > 1) & any(abs(y - round(y)) >= .Machine$double.eps^0.5)) {
                    uncal <- TRUE
                }
                if (length(seq(0, max(y))) > 20) {
                    mvuncal <- TRUE
                }
            }
            return(c(pn, uncal, mvuncal))
        })
        checknumeric <- sapply(checkNumUncal, "[[", 1)
        checkuncal <- sapply(checkNumUncal, "[[", 2)
        checkmvuncal <- sapply(checkNumUncal, "[[", 3)
        if (!all(checknumeric)) {
            cat("\n")
            notnumeric <- colnames(data)[!checknumeric]
            errmessage <- paste("The causal condition",
                                ifelse(length(notnumeric) == 1, " ", "s "),
                                paste(notnumeric, collapse=", "),
                                ifelse(length(notnumeric) == 1, " is ", " are "),
                                "not numeric.", sep="")
            stop(simpleError(paste(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep=""), "\n\n", sep = "")))
        }
        if (any(checkuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkuncal]
            errmessage <- paste("Uncalibrated data.\n",
            "Fuzzy sets should have values bound to the interval [0 , 1] and all other sets should be crisp.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
        if (any(checkmvuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkmvuncal]
            errmessage <- paste("Possibly uncalibrated data.\n",
            "Multivalue conditions with more than 20 levels are unlikely to be (properly) calibrated.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
    }
    else if (is.vector(data)) {
        if (!possibleNumeric(data)) {
            cat("\n")
            stop(simpleError("Non numeric input.\n\n"))
        }
    }
}
`verify.tt` <-
function(data, outcome = "", conditions = "", complete = FALSE, show.cases = FALSE, ic1 = 1, ic0 = 1, inf.test) {
    if (!inherits(data, "data.frame")) {
        cat("\n")
        errmessage <- paste("You have to provide a data frame, the current \"data\" argument contains an object\n",
                   "       of class \"", class(data), "\"",
                   ifelse(class(data) == "sS", ", created by superSubset()", ""),
                   ifelse(class(data) == "tt", ", created by truthTable()", ""),
                   ifelse(class(data) == "pof", ", created by pof()", ""),
                   ".\n\n", sep="")
        stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
    }
    if (methods::is(data, "tt")) {
        data <- data$initial.data
    }
    if (identical(outcome, "")) {
        cat("\n")
        stop(simpleError("You haven't specified the outcome set.\n\n"))
    }
    if (!is.element(outcome, colnames(data))) {
        cat("\n")
        stop(simpleError("The outcome's name is not correct.\n\n"))
    }
    if (!identical(conditions, "")) {
        if (length(conditions) == 1 & is.character(conditions)) {
            conditions <- splitstr(conditions)
        }
        if (is.element(outcome, conditions)) {
            cat("\n")
            stop(simpleError(paste0("Variable \"", outcome, "\" cannot be both outcome _and_ condition!\n\n")))
        }
        if (!all(conditions %in% names(data))) {
            cat("\n")
            stop(simpleError("The conditions' names are not correct.\n\n"))
        }
        if (any(duplicated(conditions))) {
            cat("\n")
            stop(simpleError("Duplicated conditions.\n\n"))
        }
    }
    else {
        conditions <- colnames(data)
        conditions <- setdiff(conditions, outcome)
    }
    if (any(is.na(data))) {
        checked <- sapply(data, function(x) any(is.na(x)))
        cat("\n")
        stop(simpleError(paste("Missing values in the data are not allowed. Please check columns:\n",
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep="")))
    }
    if (any(c(ic1, ic0) < 0) | any(c(ic1, ic0) > 1)) {
        cat("\n")
        stop(simpleError("The including cut-off(s) should be bound to the interval [0, 1].\n\n"))
    }
    data <- data[, c(conditions, outcome)]
    data <- as.data.frame(lapply(data, function(x) {
        x <- as.character(x)
        x[x %in% c("-", "dc", "?")] <- -1
        return(asNumeric(x))
    }))
    verify.qca(data)
    verify.inf.test(inf.test, data)
}
`verify.minimize` <-
function(data, outcome = "", conditions = "", explain = "",
         include = "", use.letters = FALSE) {
    verify.data(data, outcome = outcome, conditions = conditions)
    if (all(explain == "")) {
        cat("\n")
        stop(simpleError("You have not specified what to explain.\n\n"))
    }
    if (any(explain == 0)) {
        cat("\n")
        stop(simpleError("Negative output configurations cannot be explained.\n\n"))
    }
    if (any(include == 0)) {
        cat("\n")
        stop(simpleError("Negative output configurations cannot be included in the minimization.\n\n"))
    }
    if (length(setdiff(explain, c(1, "C"))) > 0) {
        cat("\n")
        stop(simpleError("Only the positive output configurations and/or contradictions can be explained.\n\n"))
    }
    if (length(setdiff(include, c("?", "C", ""))) > 0) {
        cat("\n")
        stop(simpleError("Only the remainders and/or the contradictions can be included in the minimization.\n\n"))
    }
    if (is.element("C", explain) & is.element("C", include)) {
        cat("\n")
        stop(simpleError("Contradictions are either explained or included, but not both.\n\n"))
    }
    if (!identical(conditions, "")) {
        if (length(conditions) == 1 & is.character(conditions)) {
            conditions <- splitstr(conditions)
        }
        if (is.element(outcome, conditions)) {
            cat("\n")
            stop(simpleError(paste0("\"", outcome, "\" cannot be both outcome _and_ condition.\n\n")))
        }
        if (!all(is.element(conditions, names(data)))) {
            cat("\n")
            stop(simpleError("The conditions' names are not correct.\n\n"))
        }
    }
    if (use.letters & ncol(data) > 27) {
        cat("\n")
        stop(simpleError("Cannot use letters. There are more than 26 conditions.\n\n"))
    }
    if (any(is.na(data))) {
        checked <- sapply(data, function(x) any(is.na(x)))
        cat("\n")
        stop(simpleError(paste("Missing values in the data are not allowed. Please check columns:\n",
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep="")))
    }
}
`verify.dir.exp` <-
function(data, outcome, conditions, dir.exp = "") {
    if (identical(dir.exp, "")) {
        return(dir.exp)
    }
    else {
        delc <- list()
        baselist <- vector(mode = "list", length = length(conditions))
        names(baselist) <- conditions
        for (i in seq(length(conditions))) {
            values <- sort(unique(data[, conditions[i]]))
            if (is.factor(values)) {
                values <- as.character(values)
            }
            values <- setdiff(values, c("-", "dc", "?"))
            if (!possibleNumeric(values)) {
                cat("\n")
                stop(simpleError("Data contains non-numerical values.\n\n"))
            }
            values <- asNumeric(values)
            max.value <- ifelse(any(values %% 1 > 0), 1, max(values))
            baselist[[i]] <- logical(max.value + 1)
        }
        if (length(dir.exp) == 1 & is.character(dir.exp)) {
            dir.exp <- splitstr(dir.exp)
        }
        oldway <- possibleNumeric(unlist(strsplit(gsub("-|;", "", dir.exp), split = "")))
        if (oldway) {
            delc$IDE <- baselist
            if (length(dir.exp) != length(conditions)) {
                cat("\n")
                stop(simpleError("Number of expectations does not match number of conditions.\n\n"))
            }
            del <- strsplit(as.character(dir.exp), split = ";")
            if (is.null(names(dir.exp))) {
                names(del) <- conditions
            }
            else {
                if (length(names(dir.exp)) != length(conditions)) {
                    cat("\n")
                    stop(simpleError("All directional expectations should have names, or none at all.\n\n"))
                }
                else if (length(setdiff(names(dir.exp), conditions)) > 0) {
                    cat("\n")
                    stop(simpleError("Incorect names of the directional expectations.\n\n"))
                }
                names(del) <- names(dir.exp)
                del <- del[conditions]
            }
            for (i in seq(length(del))) {
                values <- del[[i]]
                if (!all(is.element(values, c("-", "dc")))) {
                    values <- asNumeric(setdiff(values, c("-", "dc")))
                    if (length(setdiff(values, seq(length(delc$IDE[[i]])) - 1) > 0)) {
                        cat("\n")
                        errmessage <- paste("Values specified in the directional expectations do not appear in the data, for condition \"", conditions[i], "\".\n\n", sep="")
                        stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
                    }
                    else {
                        delc$IDE[[i]][values + 1] <- TRUE
                    }
                }
            }
            return(delc)
        }
        else {
            noflevels <- getInfo(data, conditions, outcome)$noflevels
            dem <- translate(sop(paste(dir.exp, collapse = "+"), snames = conditions, noflevels = noflevels), snames = conditions, noflevels = noflevels)
            checkdem <- unname(apply(dem, 1, function(x) sum(x >= 0)))
            ide <- which(checkdem == 1) 
            cde <- which(checkdem > 1) 
            if (length(ide) > 0) {
                delc$IDE <- baselist
                for (i in seq(length(ide))) {
                    demi <- asNumeric(dem[ide[i], ])
                    wdem <- which(demi >= 0)
                    delc$IDE[[wdem]][demi[wdem] + 1] <- TRUE
                }
            }
            if (length(cde) > 0) {
                delc$CDE <- list()
                for (i in seq(length(cde))) {
                    delc$CDE[[i]] <- baselist
                    demi <- asNumeric(dem[cde[i], ])
                    wdem <- which(demi >= 0)
                    for (j in seq(length(wdem))) {
                        delc$CDE[[i]][[wdem[j]]][demi[wdem[j]] + 1] <- TRUE
                    }
                }
            }
        }
        return(delc)
    }
}
`verify.mqca` <-
function(allargs) {
    data <- allargs$input
    outcome <- splitstr(allargs$outcome)
    mvoutcome <- grepl("[{]", outcome) 
    if (any(mvoutcome)) {
        outcome.value <- curlyBrackets(outcome)
        outcome <- curlyBrackets(outcome, outside = TRUE)
        if (length(setdiff(outcome, names(data))) > 0) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            errmessage <- paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
        for (i in seq(length(outcome))) {
            if (mvoutcome[i]) {
                mvnot <- setdiff(splitstr(outcome.value[i]), unique(data[, outcome[i]]))
                if (length(mvnot) > 0) {
                    cat("\n")
                    errmessage <- sprintf("Value(s) %s not found in the outcome \"%s\".\n\n", paste(mvnot, collapse = ","), outcome[i])
                    stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
                }
            }
        }
    }
    else {
        if (length(setdiff(outcome, names(data))) > 0) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            errmessage <- paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep="")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
        }
        fuzzy.outcome <- apply(data[, outcome, drop=FALSE], 2, function(x) any(x %% 1 > 0))
        if (any(!fuzzy.outcome)) {
            outcome.copy <- outcome[!fuzzy.outcome]
            for (i in outcome.copy) {
                valents <- unique(data[, i])
                if (!all(valents %in% c(0, 1))) {
                    cat("\n")
                    errmessage <- paste("Please specify the value of outcome variable \"", i, "\" to explain.\n\n", sep = "")
                    stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
                }
            }
        }
    }
    conditions <- allargs$conditions
    if (is.null(conditions)) {
        conditions <- names(data)
    }
    else {
        conditions <- splitstr(conditions)
    }
    if (length(setdiff(outcome, conditions)) > 0) {
        outcome <- setdiff(outcome, conditions)
        cat("\n")
        errmessage <- paste("Outcome(s) not present in the conditions' names: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep="")
        stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
    }
}
`verify.inf.test` <- function(inf.test, data) {
    if (all(inf.test != "")) {
        if (inf.test[1] != "binom") {
            cat("\n")
            stop(simpleError("For the moment only \"binom\"ial testing for crisp data is allowed.\n\n"))
        }
        else {
            fuzzy <- apply(data, 2, function(x) any(x %% 1 > 0))
            if (any(fuzzy)) {
                cat("\n")
                errmessage <- "The binomial test only works with crisp data.\n\n"
                stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
            }
        }
        if (length(inf.test) > 1) {
            alpha <- as.numeric(inf.test[2])
            if (is.na(alpha) | alpha < 0 | alpha > 1) {
                cat("\n")
                errmessage <- "The second value of inf.test should be a number between 0 and 1.\n\n"
                stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep="")))
            }
        }
    }
}
`verify.multivalue` <- function(expression, snames = "", noflevels, data) {
    if (length(unlist(gregexpr("[{]+", expression))) != length(unlist(gregexpr("[}]+", expression)))) {
        cat("\n")
        stop(simpleError("Incorrect expression, opened and closed brackets don't match.\n\n"))
    }
    tempexpr <- gsub("[*|,|;|(|)]", "", expression)
    pp <- unlist(strsplit(tempexpr, split="[+]"))
    insb <- curlyBrackets(gsub("[*|(|)]", "", expression))
    tempexpr <- curlyBrackets(tempexpr, outside=TRUE)
    if (length(insb) != length(tempexpr)) {
        cat("\n")
        stop(simpleError("Incorrect expression, some snames don't have brackets.\n\n"))
    }
    if (any(grepl("[a-zA-Z]", gsub("[,|;]", "", insb)))) {
        cat("\n")
        stop(simpleError("Invalid {multi}values, levels should be numeric.\n\n"))
    }
    conds <- sort(unique(toupper(notilde(curlyBrackets(pp, outside = TRUE)))))
    if (missing(data)) {
        if (missing(noflevels)) {
            if (any(hastilde(expression))) {
                cat("\n")
                stop(simpleError("Negating a multivalue condition requires the number of levels.\n\n"))
            }
        }
        else {
            if (identical(snames, "")) {
                cat("\n")
                stop(simpleError("Cannot verify the number of levels without the set names.\n\n"))
            }
            snames <- splitstr(snames)
            noflevels <- splitstr(noflevels)
            if (length(snames) != length(noflevels)) {
                cat("\n")
                stop(simpleError("Length of the set names differs from the length of the number of levels.\n\n"))
            }
            for (i in seq(length(tempexpr))) {
                if (!is.element(notilde(tempexpr[i]), snames)) {
                    cat("\n")
                    stop(simpleError(sprintf("Condition %s not present in the set names.\n\n", tempexpr[i])))
                }
                if (max(asNumeric(splitstr(insb[i]))) > noflevels[match(notilde(tempexpr[i]), snames)] - 1) {
                    cat("\n")
                    stop(simpleError(sprintf("Levels outside the number of levels for condition %s.\n\n", tempexpr[i])))
                }
            }
        }
    }
    else { 
        if (identical(snames, "")) {
            if (length(setdiff(conds, colnames(data))) > 0) {
                cat("\n")
                stop(simpleError("Parts of the expression don't match the column names from \"data\" argument.\n\n"))
            }
        }
    }
    if (!identical(snames, "")) {
        if (length(setdiff(conds, toupper(splitstr(snames)))) > 0) {
            cat("\n")
            stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
        }
    }
}
