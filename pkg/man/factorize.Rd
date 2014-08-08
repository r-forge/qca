\name{factorize}

\alias{factorize}

\title{Factorize Set-Theoretic Expressions}

\description{
This function finds all possibilities for factorizing a set-theoretic expression.
}

\usage{
factorize(expression, prod.split = "", sort.factorizing = FALSE, 
          sort.factorized = FALSE)
}

\arguments{
  \item{expression}{A string representing a set-theoretic expression or a solution 
        object of class \dQuote{qca}.}
  \item{prod.split}{The intersection (product) operator (if any).}
  \item{sort.factorizing}{Logical, sort results beginning with largest number of 
        factorizing sets.}
  \item{sort.factorized}{Logical, sort results beginning with largest number of 
        factorized sets.}
}

\details{
In set theory, the intersection operator \dQuote{*} is distributive over the union 
operator \dQuote{+} such that for any three sets \eqn{a}, \eqn{b} and \eqn{c}, the 
following law holds: \eqn{(a*b) + (a*c) = a*(b + c)}{(a*b) + (a*c) = a*(b + c)} 
(Hohn 1966, pp.78-80; South 1974, p.12). The \code{factorize()} function finds 
all possible sets \eqn{a}{a} for any set-theoretic expression. Factorized 
versions of the initial expression(s) can be sorted in decreasing order by the 
number of factorizing sets or in decreasing order by the number of factorized sets. 
}

\value{A list with the following components:\cr
  \item{initial}{The initial expression.}
  \item{factored}{The factorizations of the initial expression.}
}

\references{
Hohn, Franz E. 1966. \emph{Applied Boolean Algebra: An Elementary Introduction}. 
2nd ed. New York: Macmillan.

South, G. F. 1974. \emph{Boolean Algebra and Its Uses}. New York: Van Nostrand Reinhold.
}

\seealso{\code{\link{eqmcc}}}

\examples{
# factorize a union of intersections;
# if single letters are used, argument "prod.split" is not needed
factorize("AB + AC")

# but "prod.split" is needed in these cases
factorize("one*TWO*four + one*THREE + THREE*four", prod.split = "*")
factorize("~ONE*TWO*~FOUR + ~ONE*THREE + THREE*~FOUR", prod.split = "*")
factorize("one&TWO&four + one&THREE + THREE&four", prod.split = "&")

# factorize solution objects directly
data(d.HK)
HK.sol <- eqmcc(d.HK, outcome = "PB", conditions = c("C", "F", "T", "V"), 
  include = c("?"))
factorize(HK.sol)

data(d.Emm)
Emm.sol <- eqmcc(d.Emm, outcome = "JSR", incl.cut1 = 0.9)
factorize(Emm.sol)

# sort by the largest number of factoring sets
factorize(Emm.sol, sort.factorizing = TRUE)

# sort by the largest number of factorized sets
factorize(Emm.sol, sort.factorized = TRUE)
}

\keyword{functions}
