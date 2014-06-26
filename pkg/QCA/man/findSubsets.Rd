\name{findSubsets}

\alias{findSubsets}

\title{Find all Subsets of an Implicant}

\description{
This function finds all row numbers in the implicant matrix which correspond to all 
possible subsets of an implicant.
}

\usage{
findSubsets(noflevels3k, row.no, maximum)
}

\arguments{
  \item{noflevels3k}{A vector containing the number of levels (values) for 
        each variable incremented by 1.}
  \item{row.no}{The row number of the implicant in the implicant matrix.}
  \item{maximum}{The highest row number to be returned.}
}

\details{
All subsets of an implicant can be found in the implicant matrix. An implicant 
matrix consists of all truth table configurations and their supersets, including 
the empty set (Dusa 2007, 2010). The total number of implicants \eqn{q} is 
given by \eqn{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}{q = \prod_{j = 1}^{k}{(p_{j} + 1)}}, 
where \eqn{p_{j}}{p_{j}} is the number of values for condition variable 
\eqn{j} and \eqn{k} is the total number of condition variables. 

The function \code{findSubsets()} finds all row numbers in the implicant matrix 
that correspond to all possible subsets of an implicant.
}

\value{
A vector of the row numbers of all possible subsets of an implicant in the 
implicant matrix.
}

\references{
Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: WP 2007-49. 
Available from: http://www.compasss.org/wpseries/Dusa2007b.pdf.

Dusa, Adrian. 2010. A Mathematical Approach to the Boolean Minimization Problem. 
\emph{Quality & Quantity} 44 (1):99-113.

Quine, Willard V. 1952. The Problem of Simplifying Truth Functions.
\emph{American Mathematical Monthly} 59 (8):521-31.
}

\seealso{\code{\link{createMatrix}}, \code{\link{findSupersets}}, \code{\link{getRow}}}

\examples{
# three variables with two levels (values) each
noflevels <- c(2, 2, 2)

# second row of the implicant matrix corresponding 
# to the first value of the rightmost variable
getRow(noflevels = noflevels + 1, row.no = 2) - 1

# row numbers of the implicant matrix which 
# correspond to subsets of this value  
(rows <- findSubsets(noflevels = noflevels + 1, 2))

# corresponding matrix representation of these rows 
getRow(noflevels = noflevels + 1, row.no = rows) - 1

# stopping at row 20
getRow(noflevels = noflevels + 1, 
  row.no = findSubsets(noflevels + 1, 2, 20)) - 1

# reconstruct configurations from prime implicants;
# example of "developed formula" from Quine (1952, p. 524)
x <- data.frame(createMatrix(rep(3, 4)), row.names = 1:81)
x[x[,1] == 0 & x[,2] == 0 & x[,3] == 2 & x[,4] == 1,]
x[x[,1] == 2 & x[,2] == 2 & x[,3] == 2 & x[,4] == 0,]
levels <- rep(3, 4)
rows <- c(8, 79)
imp <- getRow(levels, findSubsets(levels, rows)) - 1
imp
}

\keyword{functions}
