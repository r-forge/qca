\name{demoChart}

\alias{demoChart}

\title{Create Prime Implicant Charts}

\description{
This function creates prime implicant charts. It is intended for demonstration 
purposes.  
}

\usage{
demoChart(primes = c(""), configs = c(""), prod.split = "")
}

\arguments{
  \item{primes}{A vector of prime implicants.}                                
  \item{configs}{A vector of configurations.}
  \item{prod.split}{The intersection (product) operator (if any).}
}

\details{
This function creates prime implicant (PI) charts as proposed by Quine (1952) 
and McCluskey (1956). PI charts have the prime implicants listed on the rows and 
the truth table configurations from which they have been derived listed on the 
columns. Each configuration must be covered at least once by at least one prime 
implicant. This function is intended for demonstration purposes only. When terms
consist of more than simple letters, an intersection operator must be provided. 

For proper prime implicant chart analysis after minimization, use \code{PIchart} 
component returned by \code{eqmcc()}.

}

\value{
A logical matrix whose elements indicate which prime implicants are supersets of 
which configurations.
}

\references{
McCluskey, Edward J. 1956. \dQuote{Minimization of Boolean Functions.} 
\emph{Bell Systems Technical Journal} 35 (6):1417-44.

Quine, Willard V. 1952. \dQuote{The Problem of Simplifying Truth Functions.} 
\emph{American Mathematical Monthly} 59 (8):521-31.
}

\examples{
# simple PI chart, formatted using internal function prettyTable();
PI <- c("A", "B", "c")
CO <- c("ABC", "Abc", "AbC", "aBc")
chart <- demoChart(PI, CO)
prettyTable(chart)

# more complex example
PI <- c("AB", "BC", "Ac", "aC", "abd", "bcd")
CO <- c("ABCD", "ABCd", "ABcD", "ABcd", "AbcD", "Abcd",
        "aBCD", "aBCd", "abCD", "abCd", "abcd")
chart <- demoChart(PI, CO)
prettyTable(chart)

# different intersection operators
PI <- c("AZ", "BY", "~CX")
CO <- c("AZ*BY*CX", "AZ*~BY*~CX", "AZ*~BY*CX", "~AZ*BY*~CX")
prettyTable(demoChart(PI, CO, prod.split = "*"))

CO <- gsub("*", "&", CO, fixed = TRUE)
prettyTable(demoChart(PI, CO, prod.split = "&"))
}

\keyword{functions}
