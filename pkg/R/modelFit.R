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

`modelFit` <-
function(model, theory) {
    if (!(methods::is(model, "qca") | methods::is(model, "deMorgan"))) {
        cat("\n")
        stop(simpleError("The model should be a minimization object or its negation.\n\n"))
    }
    if (is.character(theory)) {
        if (length(theory) != 1) {
            cat("\n")
            stop(simpleError("Theory should be a single character expression.\n\n"))
        }
    }
    else {
        cat("\n")
        stop(simpleError("Theory should be a character expression or its negation.\n\n"))
    }
    noflevels <- model$tt$noflevels
    snames <- model$tt$options$conditions
    if (model$tt$options$use.letters) {
        snames <- LETTERS[seq(length(snames))]
    }
    use.tilde <- model$options$use.tilde
    pims <- model$pims
    if ("i.sol" %in% names(model)) {
        pims <- lapply(model$i.sol, function(x) x$pims)
        names(pims) <- NULL
        pims <- do.call("cbind", pims)
        solutions <- lapply(model$i.sol, function(x) x$solution)
    }
    else {
        solutions <- list(model$solution)
    }
    models <- unlist(lapply(solutions, function(x) unlist(lapply(x, paste, collapse = " + "))))
    slengths <- unlist(lapply(solutions, length))
    if (is.null(names(solutions))) {
        names(models) <- "M"
        if (slengths > 1) {
            names(models) <- paste("M", seq(slengths), sep = "")
        }
    }
    else {
        mnum <- unlist(lapply(slengths, function(x) {
            mnum <- ""
            if (x > 1) {
                mnum <- seq(x)
            }
            paste("M", mnum, sep = "")
        }))
        names(models) <- paste(mnum, rep(names(solutions), slengths), sep = "-")
    }
    result <- intersections <- vector(mode = "list", length = length(models))
    arglist <- list(snames = snames, use.tilde = use.tilde, noflevels = noflevels)
    for (i in seq(length(models))) {
        expression <- models[i]
        cpims <- pims[, unlist(strsplit(expression, split = " \\+ ")), drop = FALSE]
        cpims$MODEL <- compute(expression, data = model$tt$initial.data)
        cpims$THEORY <- compute(theory, data = model$tt$initial.data)
        intersections <- rep("", 4)
        intersections[1] <- do.call("intersection", c(list(theory, expression), arglist))
        intersections[2] <- do.call("intersection", c(list(negate(theory, snames = snames), expression), arglist))
        intersections[3] <- do.call("intersection", c(list(theory, negate(expression, snames = snames)), arglist))
        intersections[4] <- do.call("intersection", c(list(negate(theory, snames = snames), negate(expression, snames = snames)), arglist))
        intnms <- c("MODEL*THEORY", "MODEL*theory", "model*THEORY", "model*theory")
        for (nm in seq(4)) {
            int <- intersections[nm]
            if (int == "") {
                cpims[[intnms[nm]]] <- rep(0, nrow(model$tt$initial.data))
            }
            else {
                cpims[[intnms[nm]]] <- compute(int, data = model$tt$initial.data)
            }
        }
        intersections[intersections == ""] <- "-"
        names(intersections) <- intnms
        pofobj <- pof(cpims, model$tt$initial.data[, model$tt$options$outcome], relation = "sufficiency")
        pofobj$incl.cov <- pofobj$incl.cov[, 1:3]
        pofobj$incl.cov[is.na(pofobj$incl.cov[, 1]), 3] <- NA
        pofobj$modelfit <- list(model = expression, theory = theory, intersections = intersections)
        result[[i]] <- pofobj
    }
    if (length(result) == 1) {
        return(result[[1]])
    }
    return(structure(result, class = "modelFit"))
}
