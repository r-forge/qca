\name{solveChart}

\alias{solveChart}

\title{Eliminate Redundant Prime Implicants}

\description{
This function reduces the complete union of prime implicants to a minimal union 
by eliminating redundant prime implicants.
}

\usage{
solveChart(chart, row.dom = FALSE, min.dis = TRUE, ...)
}

\arguments{
  \item{chart}{A prime implicant chart.}
  \item{row.dom}{Logical, impose row dominance as constraint on solution to
        eliminate dominated inessential prime implicants.}
  \item{min.dis}{Logical, impose minimal disjunctivity as constraint on solution
        to eliminate models with more prime implicants than the model(s) with
        the fewest prime implicants.}
  \item{...}{Other arguments (for backward compatibility).}
}

\details{
This function solves prime implicant charts. Prime implicant charts have the prime 
implicants listed on the rows and the truth table configurations from which they 
have been derived listed on the columns (Quine 1952; McCluskey 1956). Each 
configuration must be covered at least once by at least one prime implicant.
}

\value{
A matrix containing in its columns the row index values from the prime implicant 
chart that form solutions according to the values specified by the user in the 
function's arguments.
}

\references{
McCluskey, Edward J. 1956. Minimization of Boolean Functions. 
\emph{Bell Systems Technical Journal} 35 (6):1417-44.

Quine, Willard V. 1952. The Problem of Simplifying Truth Functions. 
\emph{American Mathematical Monthly} 59 (8):521-31.
}

\examples{
# simple PI chart, formatted using internal function prettyTable()
PI <- c("A", "B", "c", "D") 
CO <- c("AbCD", "AbCd", "aBcd", "aBCD") 
chart <- demoChart(PI, CO)
prettyTable(chart)

# solution:
# first or third PI: A + c
solveChart(chart)

# all non-overlapping solutions: 
# first or third PI: A + c
# second or third or fourth PI: B + c + D
solveChart(chart, min.dis = TRUE)

# a more complex example
PI <- c("AB", "BC", "Ac", "aC", "abd", "bcd")
CO <- c("ABCD", "ABCd", "ABcD", "ABcd", "AbcD", "Abcd",
        "aBCD", "aBCd", "abCD", "abCd", "abcd")
chart <- demoChart(PI, CO)
prettyTable(chart)

# AB + Ac + aC + abd
# AB + Ac + aC + bcd
# BC + Ac + aC + abd
# BC + Ac + aC + bcd
solveChart(chart)
}

\keyword{functions}
