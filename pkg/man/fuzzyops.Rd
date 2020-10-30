\name{fuzzyand, fuzzyor}

\alias{fuzzyand}
\alias{fuzzyor}

\title{Logical operations}

\description{
These functions perform logical operations \code{AND} and \code{OR}, for binary crisp or fuzzy
set membership scores.
}

\usage{
fuzzyand(..., na.rm = FALSE)

fuzzyor(..., na.rm = FALSE)
}

\arguments{
    \item{...}{Two or more numerical (calibrated) objects containing membership scores,
          or a matrix / data frame of calibrated columns.}
    \item{na.rm}{Logical, indicating whether missing values should be removed.}
}


\value{A numerical vector of class \code{"QCA_fuzzy"}, with a \code{name} attribute expression}

\author{
Adrian Dusa
}


\examples{
# -----
# Cebotari & Vink (2013, 2015)

# DEMOC*GEOCON*NATPRIDE
with(CVF, fuzzyand(DEMOC, GEOCON, NATPRIDE))

# same thing with
fuzzyand(CVF[, c(1,3,5)])

# DEMOC*~GEOCON*NATPRIDE
fa <- with(CVF, fuzzyand(DEMOC, 1 - GEOCON, NATPRIDE))
fa

attr(fa, "name")


# ETHFRACT + POLDIS
with(CVF, fuzzyor(ETHFRACT, POLDIS))

# same thing with
fuzzyor(CVF[, c(2,4)])

# ETHFRACT + ~POLDIS
fo <- with(CVF, fuzzyor(ETHFRACT, 1 - POLDIS))
fo

attr(fo, "name")

}

\keyword{functions}
