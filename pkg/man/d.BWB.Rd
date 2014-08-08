\name{d.BWB}

\docType{data}

\alias{d.BWB}

\title{Trends in Narrowing Health Inequalities in England}

\description{
This data set is from Blackman, Wistow and Byrne (2011), who analyze the determinants 
of varying progress with tackling health inequalities with respect to cancers and
cardiovascular disease among a group of 27 local authority areas in England. 
}

\usage{data(d.BWB)}

\format{
This data frame contains 27 rows (cases) and the following 18 columns (variables):

\tabular{rlll}{
[ , 1]  \tab \bold{CAN} \tab outcome:   \tab area gap for deaths before age 75 from cancers               \cr 
        \tab            \tab            \tab ("1" narrowing, "0" not narrowing)                           \cr
[ , 2]  \tab \bold{BC}  \tab condition: \tab assessments of commissioning                                 \cr 
        \tab            \tab            \tab ("1" basic, "0" not basic)                                   \cr
[ , 3]  \tab \bold{SP}  \tab condition: \tab assessments of strategic partnership working                 \cr 
        \tab            \tab            \tab ("1" less than good, "0" at least good)                      \cr
[ , 4]  \tab \bold{PH}  \tab condition: \tab assessments of public health workforce planning              \cr 
        \tab            \tab            \tab ("1" less than good, "0" at least good)                      \cr
[ , 5]  \tab \bold{PR}  \tab condition: \tab frequency of progress reviews                                \cr 
        \tab            \tab            \tab ("1" less frequent, "0" more frequent)                       \cr
[ , 6]  \tab \bold{CH}  \tab condition: \tab working culture of individual commitment and champions       \cr        
        \tab            \tab            \tab ("1" yes, "0" no)                                            \cr
[ , 7]  \tab \bold{AS}  \tab condition: \tab organisational culture                                       \cr 
        \tab            \tab            \tab ("1" aspirational, "0" comfortable or complacent)            \cr
[ , 8]  \tab \bold{LI}  \tab condition: \tab index of multiple deprivation                                \cr 
        \tab            \tab            \tab ("1" lower, "0" higher)                                      \cr
[ , 9]  \tab \bold{HS}  \tab condition: \tab spend per head on cancer programmes                          \cr 
        \tab            \tab            \tab ("1" higher, "0" lower)                                      \cr
[ , 10] \tab \bold{LC}  \tab condition: \tab crime rate                                                   \cr 
        \tab            \tab            \tab ("1" lower, "0" higher)                                      \cr
[ , 11] \tab \bold{TS}  \tab condition: \tab primary care trust performance rating                        \cr 
        \tab            \tab            \tab ("1" higher, "0" lower)                                      \cr
[ , 12] \tab \bold{CVD} \tab outcome:   \tab area gap for deaths before age 75 from cardiovascular disease\cr 
        \tab            \tab            \tab ("1" narrowing, "0" not narrowing)                           \cr
[ , 13] \tab \bold{SC}  \tab condition: \tab smoking cessation services                                   \cr 
        \tab            \tab            \tab ("1" better than basic, "0" basic)                           \cr
[ , 14] \tab \bold{PC}  \tab condition: \tab primary care services                                        \cr 
        \tab            \tab            \tab ("1" better than basic, "0" basic)                           \cr
[ , 15] \tab \bold{MP}  \tab condition: \tab a few major programmes                                       \cr 
        \tab            \tab            \tab ("1" yes, "0" no)                                            \cr
[ , 16] \tab \bold{GL}  \tab condition: \tab leadership                                                   \cr 
        \tab            \tab            \tab ("1" good or excellent, "0" less than good)                  \cr
[ , 17] \tab \bold{BA}  \tab condition: \tab budget allocation relative to target                         \cr 
        \tab            \tab            \tab ("1" higher, "0" lower)                                      \cr
[ , 18] \tab \bold{IM}  \tab condition: \tab internal migration                                           \cr 
        \tab            \tab            \tab ("1" lower, "0" higher)
}
}

\source{
Blackman, Tim, Jonathan Wistow, and David Byrne. 2011. \dQuote{A Qualitative 
Comparative Analysis of Factors Associated with Trends in Narrowing Health 
Inequalities in England.} \emph{Social Science & Medicine} 72 (12):1965-74.
DOI: \href{http://dx.doi.org/10.1016/j.socscimed.2011.04.003}{10.1016/j.socscimed.2011.04.003}.
}

\keyword{datasets}
