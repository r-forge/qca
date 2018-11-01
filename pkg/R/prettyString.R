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

`prettyString` <-
function(string.vector, string.width = 80, repeat.space = 5, separator = ",", sufnec = "",
         outcome = "", cases = FALSE) {
    if (length(string.vector) == 1) {
        if (nchar(encodeString(paste(string.vector, " ", sufnec, " ", outcome, sep=""))) >= string.width) {
            string.vector <- unlist(strsplit(string.vector, split = paste(" \\", separator, " ", sep = ""), useBytes = TRUE))
        }
    }
    string <- string.vector[1]
    if (length(string.vector) > 1) {
        startpoint <- 1
        for (j in seq(2, length(string.vector) + 1)) {
            if (j <= length(string.vector)) {
                if (nchar(encodeString(paste(string.vector[seq(startpoint, j - ifelse(separator == ";", 1, 0))], collapse = paste(ifelse(separator == ";", "", " "), separator, " ", sep="")))) >= string.width) {
                        string <- paste(paste(string, ifelse(separator == ";", "", " "), separator, "\n", sep = ""), 
                                        paste(rep(" ", repeat.space), collapse=""),
                                        string.vector[j], sep="")
                    startpoint <- j
                }
                else {
                    string <- paste(string, ifelse(separator == ";", "", " "), separator, " ", string.vector[j], sep = "")
                }
            }
            else {
                if (outcome != "") {
                    last.part <- paste(paste(string.vector[seq(startpoint, j - 1)], collapse = paste(ifelse(separator == ";", "", " "), separator, " ", sep="")), sep="")
                    if (nchar(encodeString(paste(last.part, " ", sufnec, " ", outcome, sep = ""))) >= string.width) {
                        string <- paste(paste(string, "\n", sep=""),
                                  paste(rep(" ", repeat.space), collapse=""),
                                  sufnec, " ", outcome, sep = "")
                    }
                    else {
                        string <- paste(string, " ", sufnec, " ", outcome, sep = "")
                    }
                }
            }
        }
    }
    else {
        if (outcome != "") {
            string <- paste(string, " ", sufnec, " ", outcome, sep = "")
        }
    }
    return(string)
}
