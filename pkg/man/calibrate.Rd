\name{calibrate}

\alias{calibrate}

\title{Calibrate Crisp and Fuzzy Set Variables}

\description{
This function produces crisp and fuzzy sets from base variables and some specified 
threshold(s). The calibration of fuzzy sets is possible for positive and negative 
end-point and mid-point concepts, using the method of transformational assignment.
}

\usage{
calibrate(x, type = "crisp", thresholds = NA, include = TRUE, 
          logistic = FALSE, idm = 0.95, ecdf = FALSE, p = 1, q = 1)
}

\arguments{
  \item{x}{An interval or ratio-scaled base variable.}
  \item{type}{The calibration type, either "crisp" or "fuzzy".}
  \item{thresholds}{A vector of thresholds.}
  \item{include}{Logical, include threshold(s) in the set (\code{type = "crisp"} 
        only).}
  \item{logistic}{Calibrate to fuzzy sets using the logistic function.}
  \item{idm}{The set inclusion degree of membership for the logistic function.}
  \item{ecdf}{Calibrate to fuzzy sets using the empirical cumulative distribution 
        function of the base variable.}
  \item{p}{Parameter: if \eqn{p > 1} concentration, if \eqn{0 < p < 1} dilation 
           below crossover.}
  \item{q}{Parameter: if \eqn{q > 1} dilation, if \eqn{0 < q < 1} concentration 
           above crossover.}
}

\details{
Calibration is the process by which set membership scores are assigned to cases.
With interval and ratio-scaled base variables, calibration can be based on
transformational assignments using (piecewise-defined) membership functions.

For \code{type = "crisp"}, one threshold produces two crisp sets: 0 and 1. 
More thresholds produce multiple crisp sets. For example, two thresholds 
produce three crisp sets: 0, 1 and 2.

For \code{type = "fuzzy"}, this function can generate fuzzy sets by linear, 
s-shaped, inverted s-shaped and logistic transformation for end-point concepts. 
It can generate fuzzy sets by trapezoidal, triangular and bell-shaped transformation
for mid-point concepts (Bojadziev and Bojadziev 2007; Clark et al. 2008; Thiem 2014; 
Thiem and Dusa 2013).

For calibrating fuzzy sets based on end-point concepts, \code{thresholds} should 
be specified as a numeric vector \code{c(thEX, thCR, thIN)}, where 
\code{thEX} is the threshold for full set exclusion, \code{thCR} the threshold 
for set crossover, and \code{thIN} the threshold for full set inclusion.

If \code{thEX} \eqn{<} \code{thCR} \eqn{<} \code{thIN}, then the membership 
function is increasing from \code{thEX} to \code{thIN}. If \code{thIN} \eqn{<} 
\code{thCR} \eqn{<} \code{thEX}, then the membership function is decreasing from 
\code{thIN} to \code{thEX}.

For calibrating fuzzy sets based on mid-point concepts, \code{thresholds} should be 
specified as a numeric vector \code{c(thEX1, thCR1, thIN1, thIN2, thCR2, thEX2)}, 
where \code{thEX1} is the first (left) threshold for full set exclusion, \code{thCR1} 
the first (left) threshold for set crossover, \code{thIN1} the first (left) threshold 
for full set inclusion, \code{thIN2} the second (right) threshold for full set 
inclusion, \code{thCR2} the second (right) threshold for set crossover, and 
\code{thEX2} the second (right) threshold for full set exclusion.

If \code{thEX1} \eqn{<} \code{thCR1} \eqn{<} \code{thIN1} \eqn{\le} \code{thIN2}
\eqn{<} \code{thCR2} \eqn{<} \code{thEX2}, then the membership function is first
increasing from \code{thEX1} to \code{thIN1}, then flat between \code{thIN1} and 
\code{thIN2}, and finally decreasing from \code{thIN2} to \code{thEX2}. In contrast, 
if \code{thIN1} \eqn{<} \code{thCR1} \eqn{<} \code{thEX1} \eqn{\le} \code{thEX2}
\eqn{<} \code{thCR2} \eqn{<} \code{thIN2}, then the membership function is first 
decreasing from \code{thIN1} to \code{thEX1}, then flat between \code{thEX1} and 
\code{thEX2}, and finally increasing from \code{thEX2} to \code{thIN2}.

The parameters \code{p} and \code{q} control the degree of concentration and 
dilation. They should be left at their default values unless good reasons for 
changing them exist.

If \code{logistic = TRUE}, the argument \code{idm} specifies the inclusion degree 
of membership in the set.

If \code{ecdf = TRUE}, calibration is based on the empirical cumulative distribution 
function of \code{x}.
}

\value{
A numeric vector of set membership scores between 0 and 1 for bivalent crisp and
fuzzy set variables, or a numeric vector of set values for multivalent set variables 
(beginning with 0 at increments of 1).
}

\references{
Bojadziev, George, and Maria Bojadziev. 2007. \emph{Fuzzy Logic for Business, Finance, 
and Management}. 2nd ed. Hackensack, NJ: World Scientific.

Clark, Terry D., Jennifer M. Larson, John N. Mordeson, Joshua D. Potter, and 
Mark J. Wierman. 2008. \emph{Applying Fuzzy Mathematics to Formal Models in Comparative 
Politics}. Berlin: Springer.

Thiem, Alrik. 2014. \dQuote{Membership Function Sensitivity of Descriptive Statistics 
in Fuzzy-Set Relations.} \emph{International Journal of Social Research Methodology} 
17 (6):625-42. 
DOI: \href{http://dx.doi.org/10.1080/13645579.2013.806118}{10.1080/13645579.2013.806118}.

Thiem, Alrik, and Adrian Dusa. 2013. \emph{Qualitative Comparative Analysis with R: 
A User's Guide}. New York: Springer.
}

\examples{
# base variable; random draw from standard normal distribution
set.seed(30)
x <- rnorm(30)

# calibration thresholds
th <- quantile(x, seq(from = 0.1, to = 0.9, length = 5))

# calibration of bivalent crisp set variables
calibrate(x, thresholds = th[3])

# calibration of trivalent crisp set variables
calibrate(x, thresholds = c(th[2], th[4]))

# fuzzy-set calibration (positive end-point concept, linear)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5])), 
  ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (positive and corresponding negative
# end-point concept, logistic)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]), 
  logistic = TRUE, idm = 0.99), ylab = "Fuzzy Set Membership")
points(x, calibrate(x, type = "fuzzy", thresholds = c(th[5], th[3], th[1]),
  logistic = TRUE, idm = 0.99))

# fuzzy-set calibration (positive end-point concept, ECDF)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[1], th[3], th[5]), 
  ecdf = TRUE), ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (negative end-point concept, s-shaped)
plot(x, calibrate(x, type = "fuzzy", thresholds = c(th[5], th[3], th[1]), 
  p = 2, q = 2), ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (positive mid-point concept, triangular)
plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(1,2,3,3,4,5)]),
  ylab = "Fuzzy Set Membership")

# fuzzy-set calibration (negative mid-point concept, bell-shaped)
plot(x, calibrate(x, type = "fuzzy", thresholds = th[c(3,2,1,5,4,3)],
  p = 3, q = 3), ylab = "Fuzzy Set Membership")
}

\keyword{functions}
