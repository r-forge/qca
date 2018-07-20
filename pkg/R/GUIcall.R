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

`GUIcall` <- function(type = "", ...) {
    jsonify <- function(x) {
        nms <- names(x)
        result <- 'result = { '
        for (i in seq(length(x))) {
            if (is.vector(x[[i]])) {
                collapse <- ', '
                prefix <- ''
                if (is.character(x[[i]])) {
                    collapse <- '", "'
                    prefix <- '"'
                }
                result <- paste(result,
                    sprintf('"%s": [ %s%s%s ]', nms[i], prefix, paste(x[[i]], collapse = collapse), prefix)
                )
            }
            else if (is.list(x[[i]])) {
            }
            if (i < length(x)) {
                result <- paste(result, ',')
            }
        }
        result <- paste(result, '};')
        cat(result)
    }
    test <- function(...) {
        jsonify(list(...))
    }
    switch(type,
        test = test(... = ...)
    )
}
