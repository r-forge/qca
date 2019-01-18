\name{intersection}

\alias{intersection}

\title{Intersect expressions}

\description{
This function takes two or more QCA expressions (combinations of conjunctions and
disjunctions) or even entire minimization objects, and finds their intersection.
}

\usage{
intersection(..., snames = "", use.tilde = FALSE, noflevels)
}

\arguments{
  \item{...}{One or more expressions, combined with / or minimization objects
  of class \code{"qca"}.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{use.tilde}{Logical, use tilde to negate bivalent conditions.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  
}

\details{
The initial aim of this function was to provide a software implementation of the
intersection examples presented by Ragin (1987: 144-147). That type of example can also
be performed with the function \bold{\code{\link{simplify}()}}, while this function is now
mainly used in conjunction with the \bold{\code{\link{modelFit}()}} function, to assess
the intersection between theory and a QCA model.

Irrespective of the input type (character expressions and / or minimiation objects), this
function is now a wrapper to the main \bold{\code{\link{simplify}()}} function (which only
accepts character expressions).

It can deal with any kind of expressions, but multivalent crisp conditions need additional
information about their number of levels, via the argument \bold{\code{noflevels}}.

The expressions can be formulated in terms of either lower case - upper case notation
for the absence and the presence of the causal condition, or use the tilde notation
(see examples below). Usage of either of these is automatically detected, as long as all
expressions use the same notation.

If the \bold{\code{snames}} argument is provided, the result is sorted according to the order
of the causal conditions (set names) in the original dataset, otherwise it sorts the causal
conditions in alphabetical order.

For minimzation objects of class \code{"qca"}, the number of levels, and the set names are
automatically detected.
}

\author{
Adrian Dusa
}


\references{
Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.
}


\examples{
# using minimization objects
data(LF)
ttLF <- truthTable(LF, outcome = "SURV", incl.cut = 0.8)

pLF <- minimize(ttLF, include = "?")


# for example the intersection between the parsimonious model and
# a theoretical expectation

intersection(pLF, "DEV*STB")

# E1: (DEV*ind + URB*STB)*DEV*STB
#   I1: DEV*ind*STB + DEV*URB*STB


# negating the model
intersection(negate(pLF), "DEV*STB")

# E1: (dev*urb + dev*stb + urb*IND + IND*stb)*DEV*STB
#   I1: DEV*IND*STB*urb


# -----
# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)
intersection("lw", "SG + LW", snames = "S, L, W, G")

# E1: lw(SG + LW)
#   I1: SlwG


# resources for size and wealth (C = SW) with E (page 145)
intersection("SW", "SG + LW", snames = "S, L, W, G")

# E1: SW(SG + LW)
#   I1: SWG + SLW


# and factorized
factorize(intersection("SW", "SG + LW", snames = "S, L, W, G"))

# M1: SWG + SLW 
#   F1: SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
intersection("Lg", "SG + LW", snames = "S, L, W, G")

# E1: Lg(SG + LW)
#   I1: LWg
}

\keyword{functions}

