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

`export` <-
function(x, file = "", ...) {
    export.args <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]
    caseid <- "cases"
    if (any(names(export.args) == "caseid")) {
        caseid <- export.args[["caseid"]]
        Call[["caseid"]] <- NULL
    }
    if (!missing(x)) {
        if (is.data.frame(x) | is.matrix(x)) {
            if (any(rownames(x) != seq(nrow(x)))) {
                if (all(colnames(x) != caseid)) {
                    x <- cbind("cases" = rownames(x), x)
                    names(x)[1] <- caseid
                }
            }
        }
    }
    Call[["x"]] <- x
    if (any(names(export.args) == "row.names")) {
        warning("The argument \"row.names\" is set to FALSE by default.", domain = NA)
    }
    if (any(names(export.args) == "sep")) {
        if (export.args[["sep"]] == "tab") {
            export.args[["sep"]] <- "\t"
        }
        Call[["sep"]] <- export.args[["sep"]]
    }
    else {
        Call[["sep"]] <- ","
    }
    if (any(names(export.args) == "col.names")) {
        Call[["col.names"]] <- export.args[["col.names"]]
    }
    Call[["row.names"]] <- FALSE
    do.call("write.table", Call)
}
