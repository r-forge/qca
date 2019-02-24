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

`compute` <-
function(expression = "", data = NULL, separate = FALSE) { 
    expression <- gsub("[[:space:]]", "", expression)
    enchar <- nchar(expression)
    if (identical(substring(expression, 1, 2), "~(") & identical(substring(expression, enchar, enchar), ")")) {
        expression <- paste("1-", substring(expression, 3, enchar - 1), sep = "")
    }
    negated <- identical(unname(substring(expression, 1, 2)), "1-")
    expression <- gsub("1-", "", expression)
    if (is.null(data)) {
        syscalls <- parse(text = paste(unlist(lapply(sys.calls(), deparse)), collapse = "\n"))
        if (any(withdata <- grepl("with\\(", syscalls))) {
            withdata <- which(withdata)
            withdata <- withdata[length(withdata)]
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls[withdata]), split = ","))[1], envir = length(syscalls) - withdata)
        }
        else {
            colnms <- colnames(validateNames(notilde(expression), sort(toupper(eval.parent(parse(text = "ls()", n = 1))))))
            data <- vector(mode = "list", length = length(colnms))
            for (i in seq(length(data))) {
                data[[i]] <- eval.parent(parse(text = sprintf("get(\"%s\")", colnms[i]), n = 1))
            }
            if (length(unique(unlist(lapply(data, length)))) > 1) {
                cat("\n")
                stop(simpleError("Objects should be vectors of the same length.\n\n"))
            }
            names(data) <- colnms
            data <- as.data.frame(data)
        }
    }
    ppm <- translate(expression, data = data)
    pp <- attr(ppm, "retlist")
    retain <- apply(ppm, 2, function(x) any(x >= 0))
    pp <- lapply(pp, function(x) x[retain])
    ppm <- ppm[, retain, drop = FALSE]
    data <- data[, retain, drop = FALSE]
    colnames(data) <- toupper(colnames(data))
    infodata <- getInfo(cbind(data, YYYYY_YYYYY = 1), conditions = colnames(data))
    if (any(infodata$hastime)) {
        data <- infodata$data[, colnames(data), drop = FALSE]
    }
    verify.qca(data)
    tempList <- vector("list", length(pp))
    for (i in seq(length(pp))) {
        x <- which(ppm[i, ] >= 0)
        val <- pp[[i]][x]
        temp <- data[, colnames(ppm)[x], drop = FALSE]
        for (j in seq(length(val))) {
            if (!is.numeric(temp[, j]) & possibleNumeric(temp[, j])) {
                temp[, j] <- asNumeric(temp[, j])
            }
            if (any(abs(temp[, j] - round(temp[, j])) >= .Machine$double.eps^0.5)) { 
                if (length(val[[j]]) > 1) {
                    cat("\n")
                    stop(simpleError("Multiple values specified for fuzzy data.\n\n"))
                }
                if (val[[j]] == 0) {
                    temp[, j] <- 1 - temp[, j]
                }
            }
            else { 
                temp[, j] <- as.numeric(is.element(temp[, j], val[[j]]))
            }
        }
        if (ncol(temp) > 1) {
            temp <- fuzzyand(temp)
        }
        tempList[[i]] <- temp
    }
    res <- as.data.frame(matrix(unlist(tempList), ncol = length(tempList)))
    colnames(res) <- rownames(ppm)
    if (ncol(res) > 1) {
        if (!separate) {
            res <- as.vector(fuzzyor(res))
        }
    }
    else {
        res <- as.vector(res[, 1])
    }
    if (negated) res <- 1 - res
    return(res)
}
