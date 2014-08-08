\name{findSupersets}

\alias{findSupersets}

\title{Find all Supersets of an Implicant}

\description{
This function finds all possible unique supersets for a specific set of configurations 
in the implicant matrix.
}

\usage{
findSupersets(noflevels3k, input.combs)
}

\arguments{
  \item{noflevels3k}{A vector containing the number of levels (values) for each variable incremented by 1.}
  \item{input.combs}{A matrix of configurations or a vector of their row numbers in the implicant matrix.}
}

\details{
All supersets of an implicant can be found in the implicant matrix. An implicant 
matrix consists of all truth table configurations and their supersets, including 
the empty set (Dusa 2007, 2010). The total number of implicants \eqn{q}{q} is given 
by \eqn{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}, 
where \eqn{p_{j}}{p_{j}} is the number of values for condition variable 
\eqn{j} and \eqn{k} is the total number of condition variables. 

The function \code{findSupersets()} finds all row numbers in the implicant matrix 
that correspond to all possible supersets of an implicant.
}

\value{
A vector of the line numbers of all possible supersets of an implicant in the 
implicant matrix.
}

\references{
Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: Working Paper 
2007-49. URL: \url{http://www.compasss.org/wpseries/Dusa2007b.pdf}.

Dusa, Adrian. 2010. \dQuote{A Mathematical Approach to the Boolean Minimization 
Problem.} \emph{Quality & Quantity} 44 (1):99-113.
DOI: \href{http://dx.doi.org/10.1007/s11135-008-9183-x}{10.1007/s11135-008-9183-x}.
}

\seealso{\code{\link{findSubsets}}, \code{\link{getRow}}}

\examples{
# three variables with two levels (values) each
noflevels <- c(2, 2, 2)

# row 14 of the implicant matrix corresponding to the configuration
# containing the first value of all variables
getRow(noflevels = noflevels + 1, row.no = 14) - 1

# row numbers of the implicant matrix which correspond to supersets 
# of this configuration
(rows <- findSupersets(noflevels + 1, input.combs = 14))

# corresponding matrix representation of these rows 
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# now for rows 14 and 17
getRow(noflevels = noflevels + 1, row.no = c(14, 17)) - 1
(rows <- findSupersets(noflevels + 1, input.combs = c(14, 17)))
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# in matrix representation
(input.combs <- getRow(noflevels + 1, row.no = c(14, 17)))
findSupersets(noflevels + 1, input.combs = input.combs)

}

\keyword{functions}
