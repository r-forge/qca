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
    if (!is.element(outcome, colnames(data))) {
        cat("\n")
        stop(simpleError("The name of the outcome is not correct.\n\n"))
    }
    if (!identical(conditions, "")) {
        if (any(grepl(":", conditions))) {
            if (length(conditions) > 1) {
                cat("\n")
                stop(simpleError("Only one sequence of conditions allowed.\n\n"))
            }
            conditions <- unlist(strsplit(conditions, split = ":"))
            nms <- colnames(data)
            cs <- unlist(strsplit(conditions, split = ":"))
            conditions <- nms[seq(which(nms == cs[1]), which(nms == cs[2]))]
            if (is.element(outcome, conditions)) {
                cat("\n")
                stop(simpleError("Outcome found in the sequence of conditions.\n\n"))
            }
        }
        if (is.element(outcome, conditions)) {
            cat("\n")
            stop(simpleError(paste0("\"", outcome, "\" cannot be both outcome _and_ condition.\n\n")))
        }
        if (!all(is.element(conditions, names(data)))) {
            cat("\n")
            stop(simpleError("Conditions not found in the data.\n\n"))
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
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep = ""), call. = FALSE)
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
            pn <- admisc::possibleNumeric(x)
            uncal <- mvuncal <- FALSE
            if (pn) {
                y <- na.omit(admisc::asNumeric(x))
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
                                "not numeric.", sep = "")
            stop(simpleError(paste(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = ""), "\n\n", sep = "")))
        }
        if (any(checkuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkuncal]
            errmessage <- paste("Uncalibrated data.\n",
            "Fuzzy sets should have values bound to the interval [0 , 1] and all other sets should be crisp.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep = "")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
        }
        if (any(checkmvuncal)) {
            cat("\n")
            uncalibrated <- colnames(data)[checkmvuncal]
            errmessage <- paste("Possibly uncalibrated data.\n",
            "Multivalue conditions with more than 20 levels are unlikely to be (properly) calibrated.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep = "")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
        }
    }
    else if (is.vector(data)) {
        if (!admisc::possibleNumeric(data)) {
            cat("\n")
            stop(simpleError("Non numeric input.\n\n"))
        }
    }
}
`verify.tt` <-
function(data, outcome = "", conditions = "", complete = FALSE, show.cases = FALSE, ic1 = 1, ic0 = 1, inf.test) {
    if (!inherits(data, "data.frame")) {
        cat("\n")
        cls <- ifelse(methods::is(data, "QCA_sS"), "QCA_sS",
                ifelse(methods::is(data, "QCA_tt"), "QCA_tt",
                ifelse(methods::is(data, "QCA_pof"), "QCA_tt",
                paste(class(data), collapse = ", "))))
        errmessage <- paste("You have to provide a data frame, the current \"data\" argument contains an object\n",
                   "       of class ", class(data),
                   ifelse(cls == "QCA_sS", ", created by superSubset()", ""),
                   ifelse(cls == "QCA_tt", ", created by truthTable()", ""),
                   ifelse(cls == "QCA_pof", ", created by pof()", ""),
                   ".\n\n", sep = "")
        stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
    }
    if (methods::is(data, "QCA_tt")) {
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
            conditions <- admisc::splitstr(conditions)
            if (any(grepl(":", conditions)) & length(conditions) > 1) {
                cat("\n")
                stop(simpleError("Only one sequence of conditions allowed.\n\n"))
            }
            conditions <- unlist(strsplit(conditions, split = ":"))
        }
        if (is.element(outcome, conditions)) {
            cat("\n")
            stop(simpleError(paste0("Variable \"", outcome, "\" cannot be both outcome _and_ condition!\n\n")))
        }
        if (!all(is.element(conditions, names(data)))) {
            cat("\n")
            stop(simpleError("Conditions not found in the data.\n\n"))
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
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep = "")))
    }
    if (any(c(ic1, ic0) < 0) | any(c(ic1, ic0) > 1)) {
        cat("\n")
        stop(simpleError("The including cut-off(s) should be bound to the interval [0, 1].\n\n"))
    }
    data <- data[, c(conditions, outcome)]
    data <- as.data.frame(lapply(data, function(x) {
        x <- as.character(x)
        x[x %in% c("-", "dc", "?")] <- -1
        return(admisc::asNumeric(x))
    }))
    verify.qca(data)
    verify.inf.test(inf.test, data)
}
`verify.minimize` <-
function(data, outcome = "", conditions = "", explain = "",
         include = "", use.letters = FALSE) {
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
            conditions <- admisc::splitstr(conditions)
        }
        if (is.element(outcome, conditions)) {
            cat("\n")
            stop(simpleError(paste0("\"", outcome, "\" cannot be both outcome _and_ condition.\n\n")))
        }
        if (!all(is.element(conditions, names(data)))) {
            cat("\n")
            stop(simpleError("Conditions not found in the data.\n\n"))
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
             paste(names(checked)[checked], collapse = ", "), "\n\n", sep = "")))
    }
}
`verify.dir.exp` <-
function(data, outcome, conditions, noflevels, dir.exp = "", enter = NULL) {
    if (is.null(enter)) enter <- "\n"
    if (is.null(dir.exp)) {
        return(dir.exp)
    }
    else {
        multivalue <- any(grepl(mvregexp, dir.exp))
        if (is.character(dir.exp)) {
            dir.exp <- gsub(admisc::dashes(), "-", dir.exp)
        }
        if (identical(dir.exp, "")) {
            dir.exp <- paste(rep("-", length(conditions)), collapse = ",")
        }
        direxpsplit <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", dir.exp), split = ""))
        oldway <- (admisc::possibleNumeric(direxpsplit) | length(direxpsplit) == 0)
        if (oldway) {
            if (length(dir.exp) == 1) {
                dir.exp <- admisc::splitstr(dir.exp)
            }
            expression <- NULL
            if (length(dir.exp) != length(conditions)) {
                cat(enter)
                stop(simpleError(paste0("Number of expectations does not match number of conditions.", enter, enter)))
            }
            if (all(dir.exp == "-")) {
                return(matrix(0L, ncol = length(conditions)))
            }
            del <- strsplit(as.character(dir.exp), split = ";")
            if (is.null(names(dir.exp))) {
                names(del) <- conditions
            }
            else {
                if (length(names(dir.exp)) != length(conditions)) {
                    cat(enter)
                    stop(simpleError(paste0("All directional expectations should have names, or none at all.", enter, enter)))
                }
                else if (length(setdiff(names(dir.exp), conditions)) > 0) {
                    cat(enter)
                    stop(simpleError(paste0("Incorect names of the directional expectations.", enter, enter)))
                }
                names(del) <- names(dir.exp)
                del <- del[conditions]
            }
            for (i in seq(length(del))) {
                values <- del[[i]]
                if (any(values != "-")) {
                    values <- admisc::asNumeric(setdiff(values, "-"))
                    if (length(setdiff(values, seq(noflevels[i]) - 1)) > 0) {
                        cat(enter)
                        errmessage <- paste("Values specified in the directional expectations do not appear in the data, for condition \"", conditions[i], "\".\n\n", sep = "")
                        stop(simpleError(paste0(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = ""), enter, enter)))
                    }
                    else {
                        expression <- c(expression, paste(conditions[i], "[", values, "]", sep = ""))
                    }
                }
            }
            multivalue <- TRUE
            dir.exp <- expression
        }
        else {
            if (length(dir.exp) == 1) {
                if (!grepl("[+]", dir.exp) &  grepl("[,]", dir.exp)) {
                    if (multivalue) {
                        values <- admisc::squareBrackets(dir.exp)
                        atvalues <- paste("@", seq(length(values)), sep = "")
                        for (i in seq(length(values))) {
                            dir.exp <- gsub(values[i], atvalues[i], dir.exp)
                        }
                        dir.exp <- gsub(",", "+", dir.exp)
                        for (i in seq(length(values))) {
                            dir.exp <- gsub(atvalues[i], values[i], dir.exp)
                        }
                    }
                    else {
                        dir.exp <- gsub(",", "+", dir.exp)
                    }
                }
            }
        }
        dir.exp <- paste(dir.exp, collapse = "+") 
        if (!multivalue) {
            if (any(noflevels > 2)) {
                cat(enter)
                stop(simpleError(paste0("For multivalue data, directional expectations should be specified using square brackets.", enter, enter)))
            }
        }
        if (!oldway) {
            dir.exp <- tryCatch(admisc::simplify(expression = dir.exp, snames = conditions, noflevels = noflevels, dir.exp = TRUE), error = function(e) e, warning = function(w) w)
        }
        if (length(dir.exp) > 1) {
            cat(enter)
            stop(simpleError(paste0("Ambiguous directional expectations.", enter, enter)))
        }
        if (identical(dir.exp, "")) {
            cat(enter)
            stop(simpleError(paste0("Directional expectations cancel each other out to an empty set.", enter, enter)))
        }
        dir.exp <- admisc::translate(dir.exp, snames = conditions, noflevels = noflevels)
        return(matrix(as.integer(dir.exp) + 1L, ncol = ncol(dir.exp)))
    }
}
`verify.mqca` <-
function(allargs) {
    data <- allargs$input
    outcome <- admisc::splitstr(allargs$outcome)
    mvoutcome <- grepl(mvregexp, outcome) 
    if (any(mvoutcome)) {
        if (any(grepl("\\{", outcome))) {
            outcome.value <- admisc::curlyBrackets(outcome)
            outcome <- admisc::curlyBrackets(outcome, outside = TRUE)
        }
        else {
            outcome.value <- admisc::squareBrackets(outcome)
            outcome <- admisc::squareBrackets(outcome, outside = TRUE)
        }
        if (length(setdiff(outcome, names(data))) > 0) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            errmessage <- paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep = "")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
        }
        for (i in seq(length(outcome))) {
            if (mvoutcome[i]) {
                mvnot <- setdiff(admisc::splitstr(outcome.value[i]), unique(data[, outcome[i]]))
                if (length(mvnot) > 0) {
                    cat("\n")
                    errmessage <- sprintf("Value(s) %s not found in the outcome \"%s\".\n\n", paste(mvnot, collapse = ","), outcome[i])
                    stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
                }
            }
        }
    }
    else {
        if (length(setdiff(outcome, names(data))) > 0) {
            outcome <- setdiff(outcome, names(data))
            cat("\n")
            errmessage <- paste("Outcome(s) not present in the data: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep = "")
            stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
        }
        fuzzy.outcome <- apply(data[, outcome, drop=FALSE], 2, function(x) any(x %% 1 > 0))
        if (any(!fuzzy.outcome)) {
            outcome.copy <- outcome[!fuzzy.outcome]
            for (i in outcome.copy) {
                valents <- unique(data[, i])
                if (!all(valents %in% c(0, 1))) {
                    cat("\n")
                    errmessage <- paste("Please specify the value of outcome variable \"", i, "\" to explain.\n\n", sep = "")
                    stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
                }
            }
        }
    }
    conditions <- allargs$conditions
    if (is.null(conditions)) {
        conditions <- names(data)
    }
    else {
        conditions <- admisc::splitstr(conditions)
    }
    if (length(setdiff(outcome, conditions)) > 0) {
        outcome <- setdiff(outcome, conditions)
        cat("\n")
        errmessage <- paste("Outcome(s) not present in the conditions' names: \"", paste(outcome, collapse="\", \""), "\".\n\n", sep = "")
        stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
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
                stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
            }
        }
        if (length(inf.test) > 1) {
            alpha <- as.numeric(inf.test[2])
            if (is.na(alpha) | alpha < 0 | alpha > 1) {
                cat("\n")
                errmessage <- "The second value of inf.test should be a number between 0 and 1.\n\n"
                stop(simpleError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = "")))
            }
        }
    }
}
