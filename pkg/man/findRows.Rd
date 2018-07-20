\name{findRows}

\alias{findRows}

\title{Find untenable configurations}

\description{
This function takes a truth table as the input, and finds various types of untenable
assumptions that are usually used in function \code{\link{minimize}()} when excluding
certain remainders or some observed configurations from the minimization process.
}

\usage{
findRows(expression = "", obj, remainders = TRUE, type = 1)
}

\arguments{
  \item{expression}{String: a QCA expression written in sum of products form.}
  \item{obj}{A truth table (an object of class \code{"tt"}) or an equivalent
        numerical matrix.}
  \item{remainders}{Logical: find remainders only or also observed configurations,
        when \code{obj} is a truth table.}
  \item{type}{Numeric vector, specifying the type(s) of untenable configurations}
}

\details{
The primary purpose is to find untenable assumptions to be excluded
from the Boolean minimization process. For this reason, the input is most of the times
a truth table, but for demonstration purposes it can also be a simple matrix having
column names.

It started as a function to find rows that are subsets of a given SOP expression, and
it developed to cover even more untenable asumptions.

Subset rows can be anything, from remainders to the observed configurations (positive
output, negative output and contradictions), but the function returns only the
subset configurations which do not have a positive output.

It might ocasionally find negative output configurations or contradictions, but that
doesn't have any side effect because they are going to be excluded from the minimization
anyways, unless contradictions are included in the minimization. The only category that
really matters if they are identified or not, are the positive output configurations.

The contradictory simplifying assumptions (CSAs) are those which are used for both the
presence and the absence of the outcome, while simultaneous subset relations (SSRs)
when observed configurations are sufficient for both the presence and the absence of
the outcome. CSAs and SSRs are incoherent conterfactuals, part of a category called
Untenable Assumptions.

This function takes does what is normally done with a series of commands, in a
more integrated and systematic way.

Providing a truth table is sufficient to perform all these tasks, because a truth
table already contains all necessary information of how it was produced, most importantly
the inclusion cut-off(s). It uses the same options to produce a truth table for the
negation of the outcome (if the input truth table was created for its presence, or the
other way round), and minimizes both to inspect their simplifying assumptions to
detect which are contradictory.

If there are such simplifying assumptions in both parsimonious solutions, it means
these are contradictory, and if there are observed configurations that are
sufficient for both the presence and the absence of the outcome, these are also
incoherent because of the simultaneous subset relations problem.

The following types of untenable assumptions can be searched for:

\tabular{rl}{
    \code{0} \tab all of them\cr
    \code{1} \tab subsets of a given \bold{\code{expression}} (default)\cr
    \code{2} \tab contradictory simplifying assumptions\cr
    \code{3} \tab simultaneous subset relations\cr
}

To find contradictory simplifying assumptions, a truth table for the negated outcome is
constructed, using the \bold{\code{incl.cut}} argument from the \bold{\code{obj}}
input object. If the inclusion cut-off has a single value, the same is used for the negated
outcome, and if it has two values the second is used.
}


\value{
A numeric vector of row numbers from the truth table.
}

\author{
Adrian Dusa
}

\seealso{\code{\link{truthTable}}, \code{\link{minimize}}}

\examples{
# Lipset's binary crisp version
data(LC)
ttLC <- truthTable(LC, "SURV", show.cases = TRUE)

findRows("DEV*ind*STB", ttLC)

## all subset rows from the truth table, not only the remainders
findRows("DEV*ind*STB", ttLC, remainders = FALSE)


# Lipset's fuzzy version
data(LF)
ttLF <- truthTable(LF, outcome = "SURV", incl.cut = 0.8)

findRows(obj = ttLF, type = 2) # contradictory simplifying assumptions
}

\keyword{functions}
