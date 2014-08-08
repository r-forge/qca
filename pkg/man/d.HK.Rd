\name{d.HK}

\docType{data}

\alias{d.HK}

\title{Party Bans Sub-Saharan Africa}

\description{
This data is from Hartmann and Kemmerzell (2010), who analyze the determinants of
the introduction of party ban provisions and their actual implementation in sub-Saharan Africa using mvQCA.
}

\usage{data(d.HK)}

\format{
This data frame contains 48 rows (cases) and the following 7 columns (variables):

\tabular{rlll}{
[ , 1] \tab \bold{C}   \tab condition: \tab colonial tradition                                   \cr
       \tab            \tab            \tab ("2" British, "1" French, "0" other)                 \cr
[ , 2] \tab \bold{F}   \tab condition: \tab former regime type competition                       \cr
       \tab            \tab            \tab ("2" no, "1" limited, "0" multi-party)               \cr
[ , 3] \tab \bold{T}   \tab condition: \tab mode of transition                                   \cr
       \tab            \tab            \tab ("2" managed, "1" pacted, "0" democracy before 1990) \cr
[ , 4] \tab \bold{R}   \tab condition: \tab regime type                                          \cr
       \tab            \tab            \tab ("2" authoritarian, "1" liberalizing, "0" democratic)\cr
[ , 5] \tab \bold{V}   \tab condition: \tab ethnic violence                                      \cr
       \tab            \tab            \tab ("1" yes, "0" no)                                    \cr
[ , 6] \tab \bold{PB}  \tab outcome:   \tab party ban provisions introduced                      \cr
       \tab            \tab            \tab ("1" yes, "0" no)                                    \cr
[ , 7] \tab \bold{PBI} \tab outcome:   \tab party bans implemented                               \cr
       \tab            \tab            \tab ("1" yes, "0" no)
}
}

\note{
The row names are the official International Organization for Standardization 
(ISO) country code elements as specified in ISO 3166-1-alpha-2.
}

\source{
Hartmann, Christof, and Joerg Kemmerzell. 2010. \dQuote{Understanding Variations 
in Party Bans in Africa.} \emph{Democratization} 17 (4):642-65.
DOI: \href{http://dx.doi.org/10.1080/13510347.2010.491189}{10.1080/13510347.2010.491189}.
}

\keyword{datasets}