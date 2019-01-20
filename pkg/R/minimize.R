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

`minimize` <-
function(input, include = "", exclude = NULL, dir.exp = "",
         pi.cons = 0, pi.depth = 0, sol.cons = 0, sol.cov = 1, sol.depth = 0,
         min.pin = FALSE, row.dom = FALSE, all.sol = FALSE,
         details = FALSE, use.tilde = FALSE, method = "CCubes", ...) {
    metacall <- match.call()
    other.args <- list(...)
    back.args <- c("outcome", "conditions", "n.cut", "incl.cut", "complete", "show.cases", sort.by = "", "use.letters", "inf.test",
                   "rowdom", "direxp", "neg.out", "data", "relation", "explain", "omit")
    check.args <- pmatch(names(other.args), back.args)
    names(other.args)[!is.na(check.args)] <- back.args[check.args[!is.na(check.args)]]
    explain     <- if     (is.element("explain",     names(other.args))) other.args$explain      else "1"
    outcome     <- if     (is.element("outcome",     names(other.args))) other.args$outcome      else ""
    conditions  <- if     (is.element("conditions",  names(other.args))) other.args$conditions   else ""
    incl.cut    <- if     (is.element("incl.cut",    names(other.args))) other.args$incl.cut     else 1
    n.cut       <- ifelse (is.element("n.cut",       names(other.args)), other.args$n.cut,       1)
    complete    <- ifelse (is.element("complete",    names(other.args)), other.args$complete,    FALSE)
    show.cases  <- ifelse (is.element("show.cases",  names(other.args)), other.args$show.cases,  FALSE)
    dcc         <- ifelse (is.element("dcc",         names(other.args)), other.args$dcc,         FALSE)
    sort.by     <- ifelse (is.element("sort.by",     names(other.args)), other.args$sort.by,     "")
    use.letters <- ifelse (is.element("use.letters", names(other.args)), other.args$use.letters, FALSE)
    inf.test    <- if     (is.element("inf.test",    names(other.args))) other.args$inf.test     else ""
    relation    <- if     (is.element("relation",    names(other.args))) other.args$relation     else "sufficiency"
    neg.out     <- ifelse (is.element("neg.out",     names(other.args)), other.args$neg.out,     FALSE)
    enter       <- ifelse (is.element("enter",       names(other.args)), "",                     "\n") 
    if (is.null(exclude)) {
        if (is.element("omit", names(other.args))) {
            exclude <- other.args$omit
        }
    }
    row.dom    <- ifelse (is.element("rowdom",     names(other.args)), other.args$rowdom,    row.dom)
    dir.exp    <- if     (is.element("direxp",     names(other.args))) other.args$direxp     else dir.exp
    if (is.element("data", names(other.args))) {
        input <- other.args$data
        other.args$data <- NULL
    }
    if (any(is.element(c("min.dis", "mindis"), names(other.args)))) {
        cat(enter)
        stop(simpleError(paste0("Argument \"min.dis\" is obsolete, please use the formal argument \"all.sol\".", enter, enter)))
    }
    if (missing(input)) {
        cat(enter)
        stop(simpleError(paste0("The input (a truth table or a dataset) is missing.", enter, enter)))
    }
    else {
        if (is.matrix(input)) {
            if (is.null(colnames(input))) {
                cat(enter)
                stop(simpleError(paste0("The data should have column names.", enter, enter)))
            }
            if (any(duplicated(rownames(input)))) {
                rownames(input) <- seq(nrow(input))
            }
            input <- as.data.frame(input)
            for (i in seq(ncol(input))) {
                if (possibleNumeric(input[, i])) {
                    input[, i] <- asNumeric(input[, i])
                }
            }
        }
        if(!(is.data.frame(input) | methods::is(input, "tt"))) {
            cat(enter)
            stop(simpleError(paste0("The input should be a truth table or a dataset.", enter, enter)))
        }
    }
    print.truth.table <- details & !methods::is(input, "tt")
    if (identical(include, "")) {
        if (!identical(dir.exp, "")) {
            cat(enter)
            stop(simpleError(paste0("Directional expectations were specified, without including the remainders.", enter, enter)))
        }
    }
    if (is.character(explain) & !identical(explain, "1")) {
        explain <- splitstr(explain)
    }
    if (is.character(include) & !identical(include, "")) {
        include <- splitstr(include)
    }
    if (is.character(outcome) & !identical(outcome, "")) {
        outcome <- splitstr(outcome)
    }
    if (methods::is(input, "tt")) { 
        tt <- input
        recdata <- tt$recoded.data
        conditions <- colnames(recdata)[seq(length(tt$noflevels))]
        outcome <- colnames(recdata)[ncol(recdata)]
        indata <- tt$initial.data[, match(colnames(recdata), toupper(colnames(tt$initial.data)))]
    }
    else {
        if (identical(outcome, "")) {
            cat(enter)
            stop(simpleError(paste0("Consider creating a truth table first, or formally specify the argument \"outcome\".", enter, enter)))
        }
        if (any(c(pi.cons, sol.cons) > 0) & incl.cut[1] == 1) {
            incl.cut[1] <- min(c(pi.cons, sol.cons))
        }
        if (length(outcome) > 1) {
            return(do.call("minimizeLoop", as.list(metacall)[-1], envir = parent.frame()))
        }
        colnames(input) <- toupper(colnames(input))
        conditions <- toupper(conditions)
        outcome <- toupper(outcome)
        outcome.copy <- outcome
        indata <- input 
        if (tilde1st(outcome)) {
            neg.out <- TRUE
            outcome <- substring(outcome, 2)
        }
        if (!is.element(toupper(curlyBrackets(outcome, outside = TRUE)), colnames(input))) {
            cat(enter)
            stop(simpleError(paste0("Inexisting outcome name.", enter, enter)))
        }
        outcome.name <- ifelse (tilde1st(outcome), substring(outcome, 2), outcome)
        if (grepl("\\{|\\}", outcome)) {
            outcome.name <- curlyBrackets(outcome.name, outside = TRUE)
        }
        if (identical(conditions, "")) {
            conditions <- names(input)[-which(names(input) == outcome.name)]
        }
        else {
            conditions <- splitstr(conditions)
        }
        input <- input[, c(conditions, outcome.name)]
        verify.minimize(input, outcome.name, conditions, explain, include, use.letters)
        if (!is.element("incl.cut", names(other.args))) {
            other.args$incl.cut = incl.cut
        }
        tt <- do.call("truthTable", c(list(data = input), other.args))
        tt$initial.data <- indata
        indata <- input 
        recdata <- tt$recoded.data
        conditions <- toupper(conditions)
        outcome <- toupper(outcome.name)
        names(indata) <- c(conditions, outcome)
    }
    use.letters <- tt$options$use.letters
    show.cases <- show.cases | tt$options$show.cases 
    neg.out <- tt$options$neg.out
    output <- list()
    output$tt <- tt
    output$options$print.truth.table <- print.truth.table
    rowsNotMissing <- which(tt$tt$OUT != "?")
    if (any(tt$tt$OUT == "?")) {
        missings <- which(tt$tt$OUT == "?")
        tt$tt <- tt$tt[-missings, ]
    }
    noflevels <- tt$noflevels
    mbase <- as.integer(rev(c(1, cumprod(rev(noflevels))))[-1])
    mbaseplus <- rev(c(1, cumprod(rev(noflevels + 1))))[-1]
    alreadyletters <- sum(nchar(colnames(recdata)[-ncol(recdata)])) == ncol(recdata) - 1
    tt$tt[seq(length(conditions))] <- as.data.frame(lapply(tt$tt[seq(length(conditions))], function(x) {
        x[x %in% c("-", "dc")] <- -1
        return(asNumeric(x))
    }))
    if (!is.null(exclude)) {
        if (identical(include, "")) {
            include <- "?"
        }
        else if (!grepl("?", include)) {
            include <- c("?", splitstr(include))
        }
    }
    pos.incl <- unique(c(explain, include)) 
    subset.tt <- is.element(tt$tt[, "OUT"], explain)
    subset.pos <- is.element(tt$tt[, "OUT"], pos.incl)
    pos.matrix <- as.matrix(tt$tt[subset.pos, seq(length(noflevels))])
    rownames(pos.matrix) <- drop(pos.matrix %*% mbase) + 1
    pos.matrix <- pos.matrix + 1
    neg.matrix <- as.matrix(tt$tt[!is.element(tt$tt[, "OUT"], pos.incl), seq(length(noflevels))])
    neg.matrix <- matrix(as.numeric(neg.matrix), ncol = length(noflevels)) + 1
    rownames(neg.matrix) <- drop((neg.matrix - 1) %*% mbase) + 1
    if (sum(subset.pos) == 0) {
        cat(enter)
        stop(simpleError(paste0("None of the values in OUT is explained. Please check the truth table.", enter, enter)))
    }
    inputt <- as.matrix(tt$tt[subset.tt, seq(length(noflevels)), drop = FALSE])
    rownames(inputt) <- drop(inputt %*% mbase) + 1
    inputt <- inputt + 1
    if (nrow(tt$tt) == prod(tt$noflevels)) {
        inputcases <- tt$cases[rowsNotMissing][subset.tt]
    }
    else {
        inputcases <- tt$cases[subset.tt]
    }
    nofcases1 <- sum(tt$tt$n[tt$tt$OUT == 1])
    nofcases0 <- sum(tt$tt$n[tt$tt$OUT == 0])
    nofcasesC <- sum(tt$tt$n[tt$tt$OUT == "C"])
    tomit <- logical(nrow(pos.matrix))
    tomitinputt <- logical(nrow(inputt))
    excl.matrix <- matrix(nrow = 0, ncol = length(conditions))
    if (is.matrix(exclude)) {
        cnoflevels <- noflevels
        for (i in seq(ncol(exclude))) {
            if (any(exclude[, i] < 0)) {
                exclude[, i][exclude[, i] < 0] <- noflevels[i]
                cnoflevels[i] <- noflevels[i] + 1
            }
        }
        mbasec <- rev(c(1, cumprod(rev(cnoflevels))))[-1]
        omitrows <- drop(exclude %*% mbasec) + 1
        tomit <- is.element(rownames(pos.matrix) %in% omitrows)
        tomitinputt <- is.element(rownames(inputt) %in% omitrows)
        excl.matrix <- exclude
        neg.matrix <- rbind(neg.matrix, exclude + 1)
    }
    else if (is.vector(exclude)) {
        if (possibleNumeric(exclude)) {
            exclude <- asNumeric(exclude)
        }
        if (is.character(exclude)) {
            exclude <- findRows(exclude, conditions, noflevels) 
        }
        if (length(exclude) > 0) {
            tomit <- is.element(rownames(pos.matrix), exclude)
            tomitinputt <- is.element(rownames(inputt), exclude)
            excl.matrix <- getRow(exclude, noflevels)
            neg.matrix <- unique(rbind(neg.matrix, excl.matrix + 1))
        }
    }
    output$negatives <- sort(drop((neg.matrix - 1) %*% mbase) + 1)
    pos.matrix <- pos.matrix[!tomit, , drop = FALSE]
    inputt <- inputt[!tomitinputt, , drop = FALSE]
    inputcases <- inputcases[!tomitinputt]
    rownms <- rownames(inputt)
    if (nrow(pos.matrix) == 0) {
        cat(enter)
        stop(simpleError(paste0("Nothing to explain. Please check the truth table.", enter, enter)))
    }
    incl.rem <- is.element("?", include)
    if (nrow(neg.matrix) == 0 & incl.rem) { 
        cat(enter)
        stop(simpleError(paste0("All truth table configurations are used, all conditions are minimized.\n",
                   "       Please check the truth table.", enter, enter)))
    }
    expressions <- pos.matrix
    recdata[, conditions] <- as.data.frame(lapply(recdata[, conditions, drop = FALSE], function(x) {
        x[x %in% c("-", "?", "dc")] <- -1
        return(as.numeric(x))
    }))
    if (mv <- any(recdata[, seq(ncol(recdata) - 1)] > 1)) {
        use.tilde <- FALSE
    }
    collapse <- "*"
    changed <- FALSE
    if (use.letters & !alreadyletters) {
        colnames(expressions) <- colnames(inputt) <- colnames(pos.matrix) <- colnames(neg.matrix) <- LETTERS[seq(ncol(inputt))]
        changed <- TRUE
    }
    else {
        colnames(expressions) <- colnames(inputt) <- colnames(pos.matrix) <- colnames(neg.matrix) <- colnames(recdata[, seq(ncol(inputt)), drop = FALSE])
    }
    rownames(neg.matrix) <- (neg.matrix - 1) %*% mbase + 1
    output$initials <- writePrimeimp(inputt, mv = mv, use.tilde = use.tilde, collapse = collapse)
    if (any(c(pi.cons, sol.cons) > 0)) {
        incl.rem <- TRUE
        method <- "CCubes"
    }
    expressions <- .Call("C_QMC", expressions, noflevels, PACKAGE = "QCA")
    c.sol <- p.sol <- getSolution(expressions=expressions, mv=mv, use.tilde=use.tilde, collapse=collapse, inputt=inputt, row.dom=row.dom, initial=rownms, all.sol=all.sol, indata=indata, excl.matrix=excl.matrix, ...=...)
    if (incl.rem) {
        pos.matrix <- inputt
        if (method == "QMC") {
            expressions <- .Call("C_QMC", createMatrix(noflevels)[-output$negatives, , drop = FALSE] + 1, noflevels, PACKAGE = "QCA")
            setColnames(expressions, colnames(inputt))
        }
        else if (method == "eQMC") {
            if (nrow(neg.matrix) > 0) {
                expressions <- sort.int(setdiff(findSupersets(pos.matrix, noflevels + 1), findSupersets(neg.matrix, noflevels + 1)))
            }
            else {
                expressions <- sort.int(findSupersets(pos.matrix, noflevels + 1))
            }
            expressions <- .Call("C_removeRedundants", expressions, noflevels, mbaseplus, PACKAGE = "QCA")
            expressions <- sortExpressions(getRow(expressions, noflevels + 1))
            setColnames(expressions, colnames(inputt))
        }
        else {
            if (all.sol) {
                min.pin <- FALSE
            }
            extended.data <- as.matrix(tt$recoded.data)
            if (nrow(excl.matrix) > 0) {
                extended.data <- rbind(extended.data, cbind(excl.matrix, 0))
            }
            if (sol.cons > 0 & all.sol & sol.depth == 0) {
                sol.depth <- 5
            }
            expressions <- .Call("C_Cubes", list(
                            tt = cbind(rbind(pos.matrix, neg.matrix) - 1, rep(c(1, 0), c(nrow(pos.matrix), nrow(neg.matrix)))),
                            pi.cons = pi.cons, depth = as.integer(c(pi.depth, sol.depth)),
                            min.pin = min.pin, row.dom = row.dom, all.sol = all.sol, sol.cons = sol.cons,
                            sol.cov = sol.cov, data = extended.data, fs = tt$fs),
                            PACKAGE = "QCA")
            if (is.null(expressions)) {
                cat(enter)
                stop(simpleError(paste0("The PI chart is too complex for an exact solution.", enter, enter)))
            }
        }
        p.sol <- getSolution(expressions=expressions, mv=mv, use.tilde=use.tilde, collapse=collapse, inputt=inputt, row.dom=row.dom, initial=rownms, all.sol=all.sol, indata=indata, excl.matrix=excl.matrix, ...=...)
    }
    output$PIchart <- p.sol$mtrx
    class(output$PIchart) <- c("matrix", "pic")
    attr(output$PIchart, "PI") <- p.sol$expressions
    output$primes    <- p.sol$reduced$expressions
    output$solution  <- p.sol$solution.list[[1]]
    output$essential <- p.sol$solution.list[[2]]
    output$options$explain     <- explain
    output$options$neg.out     <- neg.out
    output$options$details     <- details
    output$options$sol.cons    <- sol.cons
    output$options$sol.cov     <- sol.cov
    output$options$relation    <- relation
    output$options$show.cases  <- show.cases
    output$options$use.letters <- use.letters
    output$options$use.tilde   <- use.tilde
    output$options$collapse    <- collapse
    expr.cases <- rep(NA, nrow(p.sol$reduced$expressions))
    tt.rows <- writePrimeimp(inputt, mv = mv, use.tilde = use.tilde, collapse = collapse)
    if (any(grepl("[*]", rownames(p.sol$reduced$expressions)))) {
        if (use.letters) {
            mtrxlines <- makeChart(primes = rownames(p.sol$reduced$expressions), snames = LETTERS[seq(length(conditions))], configs = tt.rows, mv = mv, use.tilde = use.tilde, noflevels = noflevels)
        }
        else {
            mtrxlines <- makeChart(primes = rownames(p.sol$reduced$expressions), snames = conditions, configs = tt.rows, mv = mv, use.tilde = use.tilde, noflevels = noflevels)
        }
    }
    else {
        if (use.letters) {
            mtrxlines <- makeChart(primes = rownames(p.sol$reduced$expressions), configs = tt.rows, snames = LETTERS[seq(length(conditions))], mv = mv, use.tilde = use.tilde, noflevels = noflevels)
        }
        else {
            mtrxlines <- makeChart(primes = rownames(p.sol$reduced$expressions), configs = tt.rows, snames = conditions, mv = mv, use.tilde = use.tilde, noflevels = noflevels)
        }
    }
    colnames(mtrxlines) <- colnames(p.sol$reduced$mtrx) <- rownms
    for (l in seq(length(expr.cases))) {
        expr.cases[l] <- paste(inputcases[mtrxlines[l, ]], collapse="; ")
    }
    output$inputcases <- inputcases
    conds <- conditions
    if (all(is.element(colnames(p.sol$reduced$expressions), conditions))) {
        conds <- colnames(p.sol$reduced$expressions)
    }
    else {
        if (all(is.element(colnames(p.sol$reduced$expressions), LETTERS))) {
            conds <- conditions[match(colnames(p.sol$reduced$expressions), LETTERS)]
        }
    }
    if (length(output$solution) == 1) {
        listIC <- pof(p.sol$reduced$expressions - 1, tt$options$outcome, indata, showc=TRUE, cases=expr.cases, neg.out=neg.out,
                      relation = "sufficiency", conditions = conds)
        listIC$options$show.cases <- show.cases
    }
    else {
        listIC <- pof(p.sol$reduced$expressions - 1, tt$options$outcome, indata, showc=TRUE, cases=expr.cases, neg.out=neg.out,
                      relation = "sufficiency", conditions = conds, solution.list=output$solution, essential=output$essential)
        listIC$options$show.cases <- show.cases
    }
    output$pims <- listIC$pims
    attr(output$pims, "conditions") <- conds
    listIC$pims <- NULL
    output$IC <- listIC
    output$numbers <- c(OUT1 = nofcases1, OUT0 = nofcases0, OUTC = nofcasesC, Total = nofcases1 + nofcases0 + nofcasesC)
    mtrx <- p.sol$mtrx[p.sol$all.PIs, , drop = FALSE]
    SA <- TRUE
    if (is.element("SA", names(other.args))) {
        SA <- other.args$SA
    }
    if (SA) {
        conds <- conditions
        if (tt$options$use.letters) {
            conds <- LETTERS[seq(length(conditions))]
        }
        mbaseexpr <- rev(c(1, cumprod(rev(noflevels[is.element(conds, colnames(p.sol$reduced$expressions))] + 1))))[-1]
        output$SA <- lapply(p.sol$solution.list[[1]], function(x) {
            p.expressions <- p.sol$reduced$expressions[x, , drop = FALSE]
            temp <- apply(p.expressions, 1, function(pr) {
                indices <- rev(which(pr == 0))
                tempr <- NULL
                for (k in indices) {
                    if (is.null(tempr)) {
                        tempr <- drop(mbaseexpr %*% pr) + sum(mbaseexpr[pr == 0])
                        temp2 <- tempr
                    }
                    for (lev in seq(noflevels[k] - 1)) {
                        temp2 <- c(temp2, tempr + mbaseexpr[k]*lev)
                    }
                    tempr <- temp2
                }
                return(tempr)
            })
            if (all(is.null(temp))) return(NULL)
            temp <- sort(unique(as.vector(unlist(temp))))
            temp <- temp[!is.element(temp, drop(inputt %*% mbaseplus))]
            if (length(temp) == 0) return(NULL)
            SAx <- getRow(temp + 1, noflevels + 1) - 1L
            colnames(SAx) <- colnames(inputt)
            rownames(SAx) <- drop(SAx %*% mbase) + 1
            return(SAx)
        })
        prettyNums <- formatC(seq(length(p.sol$solution.list[[1]])), digits = nchar(length(p.sol$solution.list[[1]])) - 1, flag = 0)
        if (!identical(dir.exp, "") & !identical(include, "") & !identical(c.sol$solution.list, NA)) {
            dir.exp <- verify.dir.exp(recdata, outcome, conditions, noflevels, dir.exp)
            EClist <- .Call("C_getEC", dir.exp, c.sol$expressions, c.sol$sol.matrix, p.sol$expressions, p.sol$sol.matrix, output$SA, PACKAGE = "QCA")
            i.sol <- vector("list", ncol(c.sol$sol.matrix)*ncol(p.sol$sol.matrix))
            index <- 1
            for (c.s in seq(ncol(c.sol$sol.matrix))) {
                for (p.s in seq(ncol(p.sol$sol.matrix))) {
                    names(i.sol)[index] <- paste("C", c.s, "P", p.s, sep = "")
                    i.sol[[index]]$EC <- EClist[[index]]
                    i.sol[[index]]$DC <- output$SA[[p.s]][setdiff(rownames(output$SA[[p.s]]), rownames(EClist[[index]])), , drop = FALSE]
                    i.sol[[index]]$NSEC <- matrix(ncol = ncol(EClist[[index]]), nrow = 0)
                    colnames(i.sol[[index]]$NSEC) <- colnames(EClist[[index]])
                    nsecs <- TRUE
                    while (nsecs) {
                        pos.matrix.i.sol <- unique(rbind(pos.matrix, i.sol[[index]]$EC + 1))
                        tomit <- logical(nrow(pos.matrix.i.sol))
                        if (is.matrix(exclude)) {
                            cnoflevels <- noflevels
                            for (i in seq(ncol(exclude))) {
                                if (any(exclude[, i] < 0)) {
                                    exclude[, i][exclude[, i] < 0] <- noflevels[i]
                                    cnoflevels[i] <- noflevels[i] + 1
                                }
                            }
                            tomit <- is.element(rownames(pos.matrix), drop(rev(c(1, cumprod(rev(cnoflevels))))[-1] %*% t(exclude)) + 1)
                        }
                        else if (is.vector(exclude)) {
                            if (is.numeric(exclude)) {
                                tomit <- is.element(rownames(pos.matrix), as.character(exclude))
                            }
                        }
                        pos.matrix.i.sol <- pos.matrix.i.sol[!tomit, , drop = FALSE]
                        expressions <- .Call("C_QMC", pos.matrix.i.sol, noflevels, PACKAGE = "QCA") 
                        i.sol.index <- getSolution(expressions=expressions, mv=mv, use.tilde=use.tilde, collapse=collapse, inputt=inputt, row.dom=row.dom, initial=rownms, all.sol=all.sol, indata=indata, ...=...)
                        i.sol.index$expressions <- i.sol.index$expressions[rowSums(i.sol.index$mtrx) > 0, , drop = FALSE]
                        if (nrow(i.sol[[index]]$EC) > 0) {
                            nsec <- !vector(length = nrow(i.sol[[index]]$EC))
                            for (i in seq(nrow(i.sol.index$expressions))) {
                                i.sol.PI <- i.sol.index$expressions[i, ]
                                for (j in seq(length(nsec))) {
                                    j.EC <- i.sol[[index]]$EC[j, ]
                                    if (all(i.sol.PI[i.sol.PI > 0] == (j.EC[i.sol.PI > 0] + 1))) {
                                        nsec[j] <- FALSE
                                    }
                                }
                            }
                            nsecs <- any(nsec)
                        }
                        else {
                            nsecs <- FALSE
                        }
                        if (nsecs) {
                            i.sol[[index]]$NSEC <- rbind(i.sol[[index]]$NSEC, i.sol[[index]]$EC[which(nsec), , drop = FALSE])
                            i.sol[[index]]$EC <- i.sol[[index]]$EC[-which(nsec), , drop = FALSE]
                        }
                    }
                    i.sol[[index]]$PIchart        <- i.sol.index$mtrx
                    class(i.sol[[index]]$PIchart) <- c("matrix", "pic")
                    i.sol[[index]]$c.sol          <- c.sol$solution.list[[1]][[c.s]]
                    i.sol[[index]]$p.sol          <- p.sol$solution.list[[1]][[p.s]]
                    i.sol[[index]]$solution       <- i.sol.index$solution.list[[1]]
                    i.sol[[index]]$essential      <- i.sol.index$solution.list[[2]]
                    i.sol[[index]]$primes         <- i.sol.index$reduced$expressions
                    expr.cases <- rep(NA, nrow(i.sol.index$reduced$expressions))
                    tt.rows <- writePrimeimp(inputt, mv = mv, use.tilde = use.tilde, collapse = collapse)
                    if (any(grepl("[*]", rownames(i.sol.index$reduced$expressions)))) {
                        if (use.letters) {
                            mtrxlines <- makeChart(primes = rownames(i.sol.index$reduced$expressions), configs = tt.rows, snames = LETTERS[seq(length(conditions))], mv = mv, use.tilde = use.tilde, noflevels = noflevels)
                        }
                        else {
                            mtrxlines <- makeChart(primes = rownames(i.sol.index$reduced$expressions), configs = tt.rows, snames = conditions, mv = mv, use.tilde = use.tilde, noflevels = noflevels)
                        }
                    }
                    else {
                        if (use.letters) {
                            mtrxlines <- makeChart(primes = rownames(i.sol.index$reduced$expressions), configs = tt.rows, snames = LETTERS[seq(length(conditions))], mv = mv, use.tilde = use.tilde, noflevels = noflevels)
                        }
                        else {
                            mtrxlines <- makeChart(primes = rownames(i.sol.index$reduced$expressions), configs = tt.rows, snames = conditions, mv = mv, use.tilde = use.tilde, noflevels = noflevels)
                        }
                    }
                    for (l in seq(length(expr.cases))) {
                        expr.cases[l] <- paste(inputcases[which(mtrxlines[l, ])], collapse="; ")
                    }
                    if (length(i.sol.index$solution.list[[1]]) == 1) {
                        i.sol[[index]]$IC <- pof(i.sol.index$reduced$expressions - 1, outcome, indata, showc = TRUE,
                                                 cases = expr.cases, relation = "sufficiency", neg.out = neg.out,
                                                 conditions = conditions)
                        i.sol[[index]]$IC$options$show.cases <- show.cases
                    }
                    else {
                        i.sol[[index]]$IC <- pof(i.sol.index$reduced$expressions - 1, outcome, indata, showc = TRUE,
                                                 cases = expr.cases, relation = "sufficiency", neg.out = neg.out, conditions = conditions,
                                                 solution.list = i.sol.index$solution.list[[1]], essential = i.sol.index$solution.list[[2]])
                        i.sol[[index]]$IC$options$show.cases <- show.cases
                    }
                    i.sol[[index]]$pims <- i.sol[[index]]$IC$pims
                    attr(i.sol[[index]]$pims, "conditions") <- conditions
                    i.sol[[index]]$IC$pims <- NULL
                    index <- index + 1
                }
            }
            output$i.sol <- i.sol
        }
        names(output$SA) <- paste("M", prettyNums, sep = "")
        output$SA <- lapply(output$SA, as.data.frame)
    }
    if (any(names(output) == "i.sol")) {
        for (i in seq(length(output$i.sol))) {
            output$i.sol[[i]]$EC <- as.data.frame(output$i.sol[[i]]$EC)
        }
    }
    if (!methods::is(input, "tt")) {
        output$tt$options$outcome <- outcome.copy
    }
    output$call <- metacall
    if (is.element("via.web", names(other.args))) {
        output$via.web <- TRUE
    }
    if (mv & !grepl("[{]", output$tt$options$outcome)) {
        output$tt$options$outcome <- paste(output$tt$options$outcome, "{1}", sep = "")
    }
    return(structure(output, class="qca"))
}
`minimizeLoop` <-
function(...) {
    allargs <- list(...)
    verify.mqca(allargs)
    outcome <- splitstr(allargs$outcome)
    minimize.list <- lapply(outcome, function(x) {
        allargs[["outcome"]] <- x
        return(do.call("minimize", allargs))
    })
    names(minimize.list) <- outcome
    return(structure(minimize.list, class = "mqca"))
}
`eqmcc` <- function(...) {
    .Deprecated(msg = "Function eqmcc() is deprecated, and has been renamed to minimize()\n")
    minimize(...)
}
