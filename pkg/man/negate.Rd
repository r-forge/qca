\name{negate}

\alias{negate}
\alias{deMorgan}

\title{Negate Boolean expressions}

\description{
Function to negate a SOP expression.
}

\usage{
negate(expression, snames = "", noflevels, use.tilde = FALSE)
}

\arguments{
  \item{expression}{A string representing a SOP expression, or a minimization
        object of class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{use.tilde}{Logical, use tilde for negation with bivalent variables.}
}

\details{

In Boolean algebra, there are two transformation rules named after the British
mathematician Augustus De Morgan. These rules state that:

1. The complement of the union of two sets is the intersection of their complements.

2. The complement of the intersection of two sets is the union of their complements.

In "normal" language, these would be written as:

1. \code{not (A and B) = (not A) or (not B)}

2. \code{not (A or B) = (not A) and (not B)}

Based on these two laws, any Boolean expression written in disjunctive normal form
can be transformed into its negation.

It is also possible to negate all models and solutions from the result of a
Boolean minimization from function \bold{\code{\link{minimize}()}}. The resulting object, of
class \code{"qca"}, is automatically recognised by this function.

In a SOP expression, the products should normally be split by using a star \bold{\code{*}}
sign, otherwise the sets' names will be considered the individual letters in alphabetical
order, unless they are specified via \bold{\code{snames}}.

To negate multilevel expressions, the argument \bold{\code{noflevels}} is required.

}

\value{
A list with the following two components:

\tabular{rl}{
  \bold{initial} \tab {The initial expression.}\cr
  \bold{negated} \tab {The negation of the initial expression.}\cr
}

If \bold{\code{expression}} is an object of type \code{"qca"}, the result is a list of solutions.
}

\author{
Adrian Dusa
}

\references{
Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative 
and Quantitative Strategies}. Berkeley: University of California Press.
}

\seealso{\code{\link{minimize}}}

\examples{

# example from Ragin (1987, p.99)
negate("AC + B~C")

# with different intersection operators
negate("AB*EF + ~CD*EF")


# using an object of class "qca" produced with minimize()
data(LC)
cLC <- minimize(LC, outcome = "SURV")

negate(cLC)


# parsimonious solution
pLC <- minimize(LC, outcome = "SURV", include = "?")

negate(pLC)

}

\keyword{functions}
