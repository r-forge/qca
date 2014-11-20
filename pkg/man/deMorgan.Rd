\name{deMorgan}

\alias{deMorgan}
\alias{is.deMorgan}

\title{Negate Set-Theoretic Expressions using DeMorgan's Laws}

\description{
This function negates simple or complex set-theoretic expressions using the two 
DeMorgan laws.
}

\usage{
deMorgan(expression, prod.split = "", use.tilde = FALSE)

is.deMorgan(x)
}

\arguments{
  \item{expression}{A string representing a set-theoretic expression or a solution 
                    object of class "qca".}
  \item{prod.split}{The intersection (product) operator (if any).}
  \item{use.tilde}{Logical, use tilde for negation with bivalent variables.}
  \item{x}{An object of class "deMorgan".}
}

\details{
DeMorgan's two laws posit that the complement of a union is the intersection of 
its separate complements, and the complement of an intersection is the union of 
its separate complements (Hohn 1966, p.80).

The argument \code{expression} can be any complex string representing a 
set-theoretic expression of unions and intersections, or a solution object of 
class "qca" (objects returned by \code{eqmcc()}). Expressions must not contain
values from multivalent variables.
}

\value{A list of solutions with their negations as components if \code{expression} 
       is an object of class "qca", or simply a list with the following components 
       if \code{expression} is a string:\cr
  \item{initial}{The initial expression.}
  \item{negated}{The negation of the initial expression.}
}

\references{
Hohn, Franz E. 1966. \emph{Applied Boolean Algebra: An Elementary Introduction}. 
2nd ed. New York: Macmillan.

Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative 
and Quantitative Strategies}. Berkeley: University of California Press.
}

\seealso{\code{\link{eqmcc}}}

\examples{
# example from Ragin (1987, p.99)
deMorgan("AC + B~C")

# with different intersection operators
deMorgan("A*C + B*~C", prod.split = "*")
deMorgan("A&C + B&~C", prod.split = "&")

# use solution object of class "qca" returned by eqmcc() function; 
# even with multiple models
data(d.represent)
Kro.sol <- eqmcc(d.represent, outcome = "WNP", include = "?")
deMorgan(Kro.sol)
}

\keyword{functions}
