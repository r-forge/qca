\name{getRow}

\alias{getRow}

\title{Get Specific Rows from Configuration and Implicant Matrices}

\description{
This function transforms the decimal representation of a row number into its 
corresponding combination of values in configuration matrices or into its 
corresponding combination of condition index values in implicant matrices.
}

\usage{
getRow(noflevels, row.no, zerobased = FALSE)
}

\arguments{
  \item{noflevels}{A vector containing the number of levels (values) for each variable.}
  \item{row.no}{A vector, the desired row numbers.}
  \item{zerobased}{Logical, the first row number is zero.}
}

\details{
Configuration matrices contain all unique and complete intersections that can 
be formed from all values of \eqn{k} variables. The total number of configurations
\eqn{d}{d} is given by 
\eqn{d = \prod_{j = 1}^{k}{p_{j}}}{d = \prod_{j = 1}^{k}{p_{j}}}, 
where \eqn{p_{j}}{p_{j}} is the number of values for condition variable 
\eqn{j}{j} and \eqn{k}{k} is the total number of condition variables. A 
configuration matrix is an essential part of a truth table. 

An implicant matrix consists of all truth table configurations and their 
supersets, including the empty set (Dusa 2007, 2010). The number of implicants 
\eqn{q}{q} is given by 
\eqn{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}.

The function \code{getRow()} transforms the decimal representation of a row number 
from a configuration matrix into its corresponding combination of values or from 
an implicant matrix into its corresponding combination of condition index values.
}

\value{
A matrix with the combination of values / condition index values corresponding 
to the respective row number(s).
}

\references{
Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: WP 2007-49. 
Available from: http://www.compasss.org/wpseries/Dusa2007b.pdf.

Dusa, Adrian. 2010. A Mathematical Approach to the Boolean Minimization Problem. 
\emph{Quality & Quantity} 44 (1):99-113.
}

\seealso{\code{\link{createMatrix}}, \code{\link[base]{expand.grid}}}

\examples{
# all three variables have two values
x <- c(2, 2, 2)

# rows from the configuration matrix
# 1 1 1
getRow(noflevels = x, row.no = 8)

# 0 0 0
getRow(noflevels = x, row.no = 0, zerobased = TRUE)

# same as above 
getRow(noflevels = x, row.no = 1)

# second variable has three values: 0, 1 and 2
x <- c(2, 3, 2)

# 1 2 0
getRow(noflevels = x, row.no = 11)

# the empty set in the implicant matrix 
getRow(noflevels = x, 1) - 1

}

\keyword{functions}
