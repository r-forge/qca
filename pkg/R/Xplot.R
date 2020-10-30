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

`Xplot` <- function(x, jitter = FALSE, at = pretty(x), ...) {
    other.args <- list(...)
    funargs <- unlist(lapply(match.call(), deparse)[-1])
    xname <- getName(funargs[1])
    linex <- 1.75
    jitfactor <- 0.5
    jitamount <- 0.5
    cexpoints <- 1
    cexaxis <- 0.8
    pch <- 21
    bgpoints <- NA
    if (length(testarg <- which(names(other.args) == "line")) > 0) {
        linex <- other.args$line
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "factor")) > 0) {
        jitfactor <- other.args$factor
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "amount")) > 0) {
        jitamount <- other.args$amount
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "cex")) > 0) {
        cexpoints <- other.args$cex
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "cex.axis")) > 0) {
        cexaxis <- other.args$cex.axis
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "pch")) > 0) {
        pch <- other.args$pch
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "bg")) > 0) {
        bgpoints <- other.args$bg
        other.args <- other.args[-testarg]
    }
    if (length(testarg <- which(names(other.args) == "xlab")) > 0) {
        xname <- other.args$xlab
        other.args <- other.args[-testarg]
    }
    y <- rep(1, length(x))
    if (jitter) {
        y <- jitter(y, jitfactor, jitamount)
    }
    toplot <- list(as.name("plot"), x, y)
    toplot$type <- "n"
    if (!is.null(at)) {
        toplot$xlim <- range(at)
    }
    toplot$ylim <- c(0, 2)
    toplot$xlab <- ""
    toplot$ylab <- ""
    toplot$axes <- FALSE
    if (length(other.args) > 0) {
        toplot <- c(toplot, other.args)
    }
    par(mar = c(ifelse(xname == "", 2, 3), 0.3, 0, 0))
    suppressWarnings(eval(as.call(toplot)))
    axis(1, at = at, cex.axis = cexaxis)
    title(xlab = xname, cex.lab = cexaxis + 0.1, font.lab = 2, line = linex)
    plotpoints <- list(as.name("points"), x, y, pch = pch, cex = cexpoints, bg = bgpoints)
    suppressWarnings(eval(as.call(c(plotpoints, other.args))))
}
