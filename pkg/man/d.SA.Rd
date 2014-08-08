\name{d.SA}

\docType{data}

\alias{d.SA}

\title{Determinants of High Transport Project Acceptance}

\description{
This data set is from Sager and Andereggen (2012), who analyze the determinants 
of high transport project acceptance in Switzerland using mvQCA. 
}

\usage{data(d.SA)}

\format{
This data frame contains 21 rows (cases) and the following 10 columns (variables):

\tabular{rlll}{
[ , 1]  \tab \bold{FED} \tab condition: \tab federal level                             \cr
        \tab            \tab            \tab ("2" federal, "1" cantonal, "0" municipal)\cr
[ , 2]  \tab \bold{FIN} \tab condition: \tab financial situation                       \cr
        \tab            \tab            \tab ("1" positive, "0" negative)              \cr
[ , 3]  \tab \bold{URB} \tab condition: \tab sociostructural project location          \cr
        \tab            \tab            \tab ("1" urban, "0" rural)                    \cr
[ , 4]  \tab \bold{GER} \tab condition: \tab cultural project location                 \cr
        \tab            \tab            \tab ("1" German-speaking, "0" French-speaking)\cr
[ , 5]  \tab \bold{HIS} \tab condition: \tab prior history                             \cr
        \tab            \tab            \tab ("1" yes, "0" no)                         \cr
[ , 6]  \tab \bold{COO} \tab condition: \tab planning coordination                     \cr
        \tab            \tab            \tab ("1" strong, "0" not strong)              \cr
[ , 7]  \tab \bold{PRO} \tab condition: \tab administrative professionalization        \cr
        \tab            \tab            \tab ("1" high, "0" not high)                  \cr
[ , 8]  \tab \bold{DIS} \tab condition: \tab administration's discretion               \cr
        \tab            \tab            \tab ("1" broad, "0" not broad)                \cr
[ , 9]  \tab \bold{EXP} \tab condition: \tab influence of external experts             \cr
        \tab            \tab            \tab ("1" great, "0" not great)                \cr
[ , 10] \tab \bold{ACC} \tab outcome:   \tab project acceptance                        \cr
        \tab            \tab            \tab ("1" high, "0" not high)
}
}

\source{
Sager, Fritz, and Celine Andereggen. 2012. \dQuote{Dealing With Complex Causality 
in Realist Synthesis: The Promise of Qualitative Comparative Analysis.} 
\emph{American Journal of Evaluation} 33 (1):60-78.
DOI: \href{http://dx.doi.org/10.1177/1098214011411574}{10.1177/1098214011411574}.
}

\keyword{datasets}
