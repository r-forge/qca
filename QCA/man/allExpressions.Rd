\name{allExpressions}

\alias{allExpressions}

\title{Create Implicant Matrices}

\description{
This function creates implicant matrices. An implicant matrix consists of all 
truth table configurations and their supersets, including the empty set.
}

\usage{
allExpressions(noflevels, raw = FALSE, arrange = FALSE)
}

\arguments{
  \item{noflevels}{The number of levels (values) for each condition variable.}
  \item{raw}{Logical, return implicant matrix with indicator for value elimination .}
  \item{arrange}{Logical, arrange for easier visual inspection.}
}

\details{
An implicant matrix consists of all truth table configurations and their 
supersets, including the empty set (Dusa 2007, 2010). The number of implicants 
\eqn{q}{q} is given by 
\eqn{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}, 
where \eqn{p_{j}}{p_{j}} is the number of values for condition variable 
\eqn{j}{j} and \eqn{k}{k} is the total number of condition variables.

If \code{raw = TRUE}, the indicator for value elimination (\code{-1}) is used. 
}

\value{
A matrix with \eqn{\prod_{j = 1}^{k}{(p_{j} + 1)}}{\prod_{j = 1}^{k}{(p_{j} + 1)}} 
rows and \eqn{k}{k} columns.
}

\references{
Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: WP 2007-49. 
Available from: http://www.compasss.org/wpseries/Dusa2007b.pdf.

Dusa, Adrian. 2010. A Mathematical Approach to the Boolean Minimization Problem. 
\emph{Quality & Quantity} 44 (1):99-113.
}

\seealso{
\code{\link{createMatrix}}
}

\examples{
# three condition variables with two values each;
# first row is empty set
allExpressions(noflevels = rep(2, 3))

# two condition variables with three values each
allExpressions(noflevels = rep(3, 2))

# arranged differently
allExpressions(noflevels = rep(3, 2), arrange = TRUE)

# with internal indicator for eliminated values
allExpressions(noflevels = rep(3, 2), raw = TRUE)
}

\keyword{functions}
