# Copyright (c) 2016 - 2020, Adrian Dusa
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

`superSubset` <-
function(data, outcome = "", conditions = "", relation = "necessity", incl.cut = 1,
    cov.cut = 0, ron.cut = 0, pri.cut = 0, use.letters = FALSE, depth = NULL,
    add = NULL, ...) {
    funargs <- lapply(lapply(match.call(), deparse)[-1], function(x) gsub("\"|[[:space:]]", "", x))
    dots <- list(...)
    if (missing(data)) {
        cat("\n")
        stop(simpleError("Data is missing.\n\n"))
    }
    funargs <- lapply(match.call(), deparse)
    outcome <- admisc::recreate(substitute(outcome), colnames(data))
    conditions <- admisc::recreate(substitute(conditions), colnames(data))
        neg.out <- FALSE
        if (is.element("neg.out", names(dots))) {
            neg.out <- dots$neg.out
        }
    incl.cut <- incl.cut - .Machine$double.eps ^ 0.5
    if (cov.cut > 0) {
        cov.cut <- cov.cut - .Machine$double.eps ^ 0.5
    }
    if (identical(outcome, "")) {
        cat("\n")
        stop(simpleError("The outcome was not specified.\n\n"))
    }
    if (is.character(outcome)) {
        funargs$outcome <- outcome
    }
    if (admisc::tilde1st(funargs$outcome)) {
        neg.out <- TRUE
        funargs$outcome <- substring(funargs$outcome, 2)
    }
    mvoutcome <- grepl("\\[|\\{", funargs$outcome)
    if (mvoutcome) {
        curly <- grepl("\\{", funargs$outcome)
        if (curly) {
            outcome.value <- admisc::curlyBrackets(funargs$outcome)
            funargs$outcome <- admisc::curlyBrackets(funargs$outcome, outside = TRUE)
        }
        else {
            outcome.value <- admisc::squareBrackets(funargs$outcome)
            funargs$outcome <- admisc::squareBrackets(funargs$outcome, outside = TRUE)
        }
    }
    if (is.character(outcome)) {
        if (!is.element(notilde(funargs$outcome), colnames(data))) {
            cat("\n")
            stop(simpleError("The outcome name does not exist in the data.\n\n"))
        }
        if (mvoutcome) {
            outcome <- as.numeric(is.element(data[, funargs$outcome], admisc::splitstr(outcome.value)))
        }
    }
    else {
        data[, funargs$outcome] <- outcome
    }
    if (identical(conditions, "")) {
        conditions <- names(data)[-which(names(data) == funargs$outcome)]
    }
    else {
        conditions <- admisc::splitstr(conditions)
    }
    verify.data(data, funargs$outcome, conditions)
    if (length(conditions) == 1) {
        if (grepl(":", conditions)) {
            nms <- colnames(data)
            cs <- unlist(strsplit(conditions, split = ":"))
            conditions <- nms[seq(which(nms == cs[1]), which(nms == cs[2]))]
        }
    }
    if (!(nec(relation) | suf(relation) | is.element(relation, c("sufnec", "necsuf")))) {
        cat("\n")
        stop(simpleError("The relationship should be \"necessity\", \"sufficiency\", \"sufnec\" or \"necsuf\".\n\n"))
    }
    relationcopy <- relation
    if (is.element(relation, c("sufnec", "necsuf"))) {
        cov.cut <- incl.cut
    }
    if (relation == "sufnec") {
        relation <- "sufficiency"
    }
    else if (relation == "necsuf") {
        relation <- "necessity"
    }
    replacements <- conditions
    data <- data[, c(conditions, funargs$outcome)]
    nofconditions <- length(conditions)
    if (neg.out) {
        data[, funargs$outcome] <- 1 - data[, funargs$outcome]
    }
    mv <- any(data[, conditions] > 1)
    alreadyletters <- sum(nchar(conditions)) == length(conditions)
    collapse <- "*"
    if (use.letters & !alreadyletters) {
        replacements <- LETTERS[seq(length(conditions))]
        names(replacements) <- conditions
        colnames(data)[seq(length(conditions))] <- conditions <- replacements
    }
    noflevels <- apply(data[, conditions, drop = FALSE], 2, max) + 1L
    fc <- apply(data[, conditions, drop = FALSE], 2, function(x) any(x %% 1 > 0))
    noflevels[fc] <- 2
    mbase <- c(rev(cumprod(rev(noflevels + 1L))), 1)[-1]
    noflevels[noflevels == 1] <- 2 
    if (is.null(depth)) {
        depth <- nofconditions
    }
    CMatrix <- .Call("C_superSubset",
                     as.matrix(data[, conditions]),
                     noflevels,
                     as.numeric(fc),
                     data[, funargs$outcome],
                     as.numeric(nec(relation)),
                     incl.cut,
                     cov.cut,
                     depth, PACKAGE = "QCA")
    if (nec(relation)) {
        setColnames(CMatrix[[1]], c("inclN", "RoN", "covN"))
        setColnames(CMatrix[[2]], c("inclN", "RoN", "covN"))
    }
    else {
        setColnames(CMatrix[[1]], c("inclS", "PRI", "covS"))
        setColnames(CMatrix[[2]], c("inclS", "PRI", "covS"))
    }
    prev.result <- FALSE
    lexpressions <- nrow(CMatrix[[1]])
    if (lexpressions > 0) {
        result.matrix <- CMatrix[[3]]
        rownames(result.matrix) <- expressions <- seq(lexpressions)
        colnames(result.matrix) <- conditions
        prev.result <- TRUE
        row_names <- admisc::writePrimeimp(result.matrix, mv = mv, collapse = collapse)
        rownames(CMatrix[[1]]) <- row_names
        result <- as.data.frame(CMatrix[[1]])
        mins <- CMatrix[[5]]
    }
    lexprnec <- 0
    if (nec(relation)) {
        lexprnec <- nrow(CMatrix[[2]])
        if (lexprnec + lexpressions == 0) {
            cat("\n")
            stop(simpleError(paste("\nThere are no configurations, using these cutoff values.\n\n", sep="")))
        }
        if (lexprnec > 0) {
            result.matrix2 <- CMatrix[[4]]
            rownames(result.matrix2) <- seq(lexprnec) + lexpressions
            colnames(result.matrix2) <- conditions
            row_names2 <- admisc::writePrimeimp(result.matrix2, mv = mv, collapse = "+")
            rownames(CMatrix[[2]]) <- row_names2
            mins2 <- CMatrix[[6]]
            if (prev.result) {
                result <- rbind(result, as.data.frame(CMatrix[[2]]))
                row_names <- c(row_names, row_names2)
                result.matrix <- rbind(result.matrix, result.matrix2)
                mins <- cbind(mins, mins2)
            }
            else {
                result <- as.data.frame(CMatrix[[2]])
                expressions <- seq(lexprnec)
                row_names <- row_names2
                result.matrix <- result.matrix2
                mins <- mins2
            }
        }
    }
    if (lexprnec + lexpressions == 0) {
        cat("\n")
        stop(simpleError(paste("There are no combinations with incl.cut = ", round(incl.cut, 3), " and cov.cut = ", round(cov.cut, 3), "\n\n", sep="")))
    }
    colnames(mins) <- rownames(result)
    rownames(mins) <- rownames(data)
    mins <- as.data.frame(mins)
    if (relationcopy == "sufnec") {
        colnames(result) <- c("inclS", "PRI", "inclN")
    }
    else if (relationcopy == "necsuf") {
        colnames(result) <- c("inclN", "PRI", "inclS")
    }
    if (nec(relation)) {
        tokeep <- result[, "RoN"] >= ron.cut
    }
    else {
        tokeep <- result[, "PRI"] >= pri.cut
    }
    result <- result[tokeep, , drop = FALSE]
    mins <- mins[, tokeep, drop = FALSE]
    attr(mins, "conditions") <- conditions 
    if (nrow(result) == 0) {
        cat("\n")
        stop(simpleError(paste("There are no combinations with", ifelse(nec(relation), paste("ron.cut =", round(ron.cut, 3)), paste("pri.cut =", round(pri.cut, 3))), "\n\n")))
    }
    if (!is.null(add)) {
        toadd <- pof(mins,
                     data[, outcome],
                     relation = ifelse(nec(relation), "nec", "suf"),
                     add = add)$incl.cov[, -seq(1, 4), drop = FALSE]
        if (is.function(add)) {
            if (any(grepl("function", funargs$add))) {
                funargs$add <- "X"
            }
            colnames(toadd) <- funargs$add
        }
        result <- cbind(result, toadd)
    }
    out.list <- list(incl.cov = result, coms = mins, use.letters = use.letters)
    if (use.letters & !alreadyletters) {
        out.list$letters <- replacements
    }
    out.list$options <- list(
        outcome = outcome,
        neg.out = neg.out,
        conditions = conditions,
        relation = relation,
        incl.cut = incl.cut,
        cov.cut = cov.cut,
        use.letters = use.letters
    )
    return(structure(out.list, class = "QCA_sS"))
}
