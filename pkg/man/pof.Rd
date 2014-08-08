\name{pof}

\alias{pof}
\alias{is.pof}

\title{Compute Set-Relational Parameters of Fit}

\description{
This function computes inclusion (consistency) and coverage scores.
}

\usage{
pof(setms, outcome, data, neg.out = FALSE, relation = "nec",
    inf.test = "", incl.cut1 = 0.75, incl.cut0 = 0.5, ...)

is.pof(x)
}

\arguments{
  \item{setms}{A data frame of set membership scores, or a matrix of implicants, 
        or a vector of implicant matrix line numbers.}
  \item{outcome}{The name of the outcome.}
  \item{data}{The working data set.}
  \item{neg.out}{Logical, use negation of \code{outcome}.}
  \item{relation}{The set relation to \code{outcome}, either \code{"nec"} or
        \code{"suf"}.}
  \item{inf.test}{The inference-statistical test to be performed (currently only
        \code{"binom"} for bivalent and multivalent crisp-set variables).}
  \item{incl.cut1}{The upper inclusion cut-off against which the
        empirical inclusion score is tested if \code{inf.test = "binom"}.}
  \item{incl.cut0}{The lower inclusion cut-off against which the
        empirical inclusion score is tested if \code{inf.test = "binom"}.}
  \item{...}{Other arguments (not used in this function).}
  \item{x}{An object of class "pof".}
}

\details{
The argument \code{setms} specifies a data frame of \emph{set} membership scores, 
where \emph{set} refers to any kind of set, including simple sets, combinations 
returned by \code{superSubset()} (\code{coms}), prime implicants returned by 
\code{eqmcc()} (\code{pims}) or any other compound set.

The function also accepts a matrix of implicants with the value representation 
of \code{createMatrix()}, or even a corresponding vector of implicant matrix line 
numbers.

The argument \code{outcome} specifies the outcome to be explained. Outcomes from 
multivalent variables require curly-bracket notation (\code{X{value}}).

The logical argument \code{neg.out} controls whether \code{outcome} is to be 
explained or its negation. If \code{outcome} is from a multivalent variable, 
\code{neg.out = TRUE} has the effect that the union of all remaining values 
becomes the new outcome to be explained.

The argument \code{inf.test} provides functionality for adjudicating between
rival hypotheses on the basis of inference-statistical tests. Currently, only an
exact binomial test (\code{"binom"}) is available, which requires the data to
contain only bivalent or multivalent crisp set variables. Two one-tailed tests
are performed. The null hypothesis with respect to \code{incl.cut1} is that the
empirical inclusion score of each element in \code{setms} is not lower than the
upper critical inclusion cut-off provided in \code{incl.cut1}. The null hypothesis
with respect to \code{incl.cut0} is that the empirical inclusion score of each
element in \code{setms} is not higher than the lower critical inclusion cut-off
provided in \code{incl.cut0}.
}

\references{
Emmenegger, Patrick. 2011. \dQuote{Job Security Regulations in Western Democracies: 
A Fuzzy Set Analysis.} \emph{European Journal of Political Research} 50 (3):336-64.
DOI: \href{http://dx.doi.org/10.1111/j.1475-6765.2010.01933.x}{10.1111/j.1475-6765.2010.01933.x}.

Krook, Mona Lena. 2010. \dQuote{Women's Representation in Parliament: A Qualitative 
Comparative Analysis.} \emph{Political Studies} 58 (5):886-908.
DOI: \href{http://dx.doi.org/10.1111/j.1467-9248.2010.00833.x}{10.1111/j.1467-9248.2010.00833.x}.
}

\seealso{\code{\link{eqmcc}}, \code{\link{superSubset}}}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(d.Kro)
head(d.Kro)

# first also compute negations
x.1 <- d.Kro[, 1:5]
x.2 <- 1 - x.1
names(x.2) <- tolower(names(x.1))
x <- cbind(x.1, x.2)
 
# necessity parameters of fit for all conditions
pof(x, outcome = "WNP", d.Kro)

# for the negated outcome
pof(x, outcome = "WNP", d.Kro, neg.out = TRUE)

# sufficiency parameters of fit
pof(x, outcome = "WNP", d.Kro, relation = "suf") 

# for the negated outcome
pof(x, outcome = "WNP", d.Kro, neg.out = TRUE, relation = "suf")

# exact binomial tests of sufficiency and necessity inclusion
pof(x.1, outcome = "WNP", d.Kro, relation = "suf", inf.test = "binom",
  incl.cut1 = 0.75, incl.cut0 = 0.5)

pof(x.1, outcome = "WNP", d.Kro, inf.test = "binom", incl.cut1 = 0.75,
  incl.cut0 = 0.5)

# fsQCA using Emmenegger (2011)
#------------------------------
data(d.Emm)
head(d.Emm)

# first test for minimally necessary combinations with superSubset(), 
# then check whether these combinations are also necessary for the 
# negation of the outcome
Emm.nr <- superSubset(d.Emm, outcome = "JSR", incl.cut = 0.965, 
  cov.cut = 0.6)
Emm.nr

pof(Emm.nr$coms, outcome = "JSR", d.Emm, neg.out = TRUE) 

# first derive the conservative solution, then check whether the 
# negations of the prime implicants are also sufficient for the outcome 
Emm.sc <- eqmcc(d.Emm, outcome = "JSR", incl.cut1 = 0.9, details = TRUE)
Emm.sc

pof(1 - Emm.sc$pims, outcome = "JSR", d.Emm, relation = "suf")

# parameters of fit for matrix of implicants;
# "-1" is the placeholder for an eliminated variable;
# e.g.: R*p*V and S*c*L*P*v
#      "S"  "C"  "L"  "R"  "P"  "V"
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]   -1   -1   -1    1    0    1
#[2,]    1    0    1   -1    1    0
confs <- matrix(c(-1,-1,-1, 1, 0, 1, 
                   1, 0, 1,-1, 1, 0), nrow = 2, byrow = TRUE)

pof(confs, outcome = "JSR", d.Emm, relation = "suf")

# or even vectors of line numbers from the implicant matrix
pof(c(43, 57), "JSR", d.Emm, relation = "suf")

# parameters of fit for a data frame
x <- data.frame(A = c(1,1,1,0,1), B = c(1,1,1,0,1),
                C = c(0,1,0,0,1), D = c(0,0,1,0,1),
                O = c(1,1,1,0,1))

pof(x[, -5], outcome = "O", x)

# for a single column from that data frame
pof(x$A, x$O)

# for multiple columns from that data frame
pof(x[, 1:2], outcome = "O", x)

}

\keyword{functions}
