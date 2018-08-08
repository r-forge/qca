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

combinations <- function (n, k, aloe = 0, zero = FALSE, inC = FALSE) {
    if (!is.numeric(k)) {
        cat("\n")
        stop(simpleError("Argument k should be numeric.\n\n"))
    }
    if (length(k) != 1L) {
        cat("\n")
        stop(simpleError("Argument k should be a scalar of length 1.\n\n"))
    }
    if (k < 0) {
        cat("\n")
        stop(simpleError("Argument k should be positive.\n\n"))
    }
    if (n < k) {
        cat("\n")
        stop(simpleError("Argument n should be greater than or equal to k.\n\n"))
    }
    n <- as.integer(n)
    k <- as.integer(k)
    zero <- as.integer(zero)
    if (inC) {
        .Call("C_combinations", list(n = n, k = k, aloe = aloe, zero = zero), PACKAGE = "QCA")
    }
    else {
        aloe <- as.integer(aloe)
        e <- 0L
        ncols <- as.integer(choose(n, k))
        h <- k - ncols == 1
        out <- vector(mode = "list", length = ncols)
        comb <- seq.int(k) - zero 
        comb[k] <- comb[k] - 1L
        last <- n == k
        i <- 1
        while (comb[1] != n - k + 1 || last) {
            last <- FALSE
            if (e < n - h) {
                h <- 1L
                e <- comb[k] + zero 
                comb[k] <- comb[k] + 1L
                if (comb[k] < aloe) {
                    comb[k] <- aloe
                    e <- aloe - 1
                }
            }
            else {
                e <- comb[k - h] + zero 
                h <- h + 1L
                under <- logical(h)
                for (j in seq(h)) {
                    under[j] <- (e + j - zero < aloe) 
                    comb[k - h + j] <- e + j - zero  
                }
                if (all(under)) {
                    comb[k] <- aloe
                    e <- aloe - 1
                    h <- 1L
                }
            }
            out[[i]] <- comb
            i <- i + 1
        }
        return(do.call("cbind", out[!unlist(lapply(out, is.null))]))
    }
}
