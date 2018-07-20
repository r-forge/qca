\name{factorize}

\alias{factorize}

\title{Factorize Boolean expressions}

\description{
This function finds all combinations of common factors in a Boolean expression
written in SOP - Sum Of Products form.
}

\usage{
factorize(input, snames = "", noflevels, pos = FALSE, use.tilde = FALSE, ...)
}

\arguments{
  \item{input}{A string containing the SOP expression, or an object of
        class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{pos}{Logical, if possible factorize using product(s) of sums.}
  \item{use.tilde}{Logical, use a tilde to negate the sets.}
  \item{...}{Other arguments (mainly for backwards compatibility).}
}

\details{

Factorization is a process of finding common factors in a Boolean expression,
written in a SOP - sum of products (or DNF - disjunctive normal form).
Whenever possible, the factorization can also be performed in a POS - product of sums form.

Conjunctions should preferably be indicated with a star \code{*} sign, but this is not
necessary when conditions have single letters or when the expression is expressed in
multi-value notation.


The argument \bold{\code{snames}} is only needed when conjunctions are not indicated by
any sign, and the set names have more than one letter each (see function
\bold{\code{translate()}} for more details).

The number of levels in \bold{\code{noflevels}} is needed only when negating multivalue
conditions, and it should complement the \bold{\code{snames}} argument.

If \bold{\code{input}} is an object of class \code{"qca"} (the result of the
\bold{\code{\link{minimize}()}} function), a factorization is performed
for each of the minimized solutions.

The argument \bold{\code{use.tilde}} is automatically activated if the input contains
a tilde to negate conditions.
}

\value{
A list with two components:

\tabular{rl}{
  \bold{initial} \tab {The input expression.}\cr
  \bold{factored} \tab {All possible factorizations of the input expression.}\cr
}

}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method. Moving beyond qualitative
and quantitative strategies}, Berkeley: University of California Press
}

\seealso{\code{\link{translate}}}

\examples{

# typical example with redundant conditions
factorize("AbcD + AbCd + AbCD + ABCd")

# results presented in alphabetical order
factorize("one*TWO*four + one*THREE + THREE*four")

# to preserve a certain order of the set names
factorize("one*TWO*four + one*THREE + THREE*four",
          snames = "ONE, TWO, THREE, FOUR")

factorize("~ONE*TWO*~FOUR + ~ONE*THREE + THREE*~FOUR",
          snames = "ONE, TWO, THREE, FOUR")

# using pos - products of sums
factorize("ac + aD + bc + bD", pos = TRUE)


# using an object of class "qca" produced with minimize()
data(CVF)
pCVF <- minimize(CVF, outcome = "PROTEST", incl.cut = 0.8,
                 include = "?", use.letters = TRUE)

factorize(pCVF)


# using an object of class "deMorgan" produced with negate()
factorize(negate(pCVF))


}

\keyword{functions}
