\name{retention}
\alias{retention}

\title{Compute Retention Probabilities of QCA Baseline Solutions}

\description{
This function computes retention probabilities of QCA baseline solutions 
(currently only for conservative solution types with csQCA).
}

\usage{
retention(data, outcome = "", conditions = c(""), type = "error",
          assump = "independent", n.cut = 1, incl.cut = 1, p.pert = 0.1,
          n.pert = 1)
}

\arguments{
  \item{data}{A data set of bivalent crisp-set variables.}
  \item{outcome}{The name of the outcome.}
  \item{conditions}{A vector of condition variables.}
  \item{type}{Induce errors on the outcome or delete cases.}
  \item{assump}{Assume dependent or independent perturbations.}
  \item{n.cut}{The minimum number of cases for a configuration not to be 
        considered as a logical remainder.}
  \item{incl.cut}{The minimum sufficiency inclusion score for an output function 
        value of "1".}
  \item{p.pert}{The probability of perturbation under independence.}
  \item{n.pert}{The number of perturbations under dependence.}
}

\details{
The argument \code{data} requires a suitable data set. Suitable data sets have 
the following structure: values of 0 and 1 for bivalent crisp-set variables. 

The argument \code{conditions} specifies the condition variables. If omitted, 
all variables in \code{data} are used except \code{outcome}.

The argument \code{type} specifies whether errors are to be induced in the 
outcome ("1" is recoded to "0"; "0" is recoded to "1") of cases or whether entire 
cases are to be deleted from the data.

The argument \code{assump} specifies whether the perturbations detailed in 
\code{type} occur independently of each other or whether they are dependent on
each other. Note that the assumption of dependence increases the consumption of
computational resources significantly.

Configurations that contain fewer than \code{n.cut} cases with membership scores 
above 0.5 are coded as logical remainders (\code{OUT = "?"}). If the number of 
such cases is at least \code{n.cut}, configurations with an inclusion score of 
at least \code{incl.cut} are coded positive (\code{OUT = "1"}), and configurations 
with an inclusion score below \code{incl.cut} are coded negative (\code{OUT = "0"}). 
Note that the possibility to specify contradictions as in \code{truthTable} does 
not exist.

The argument \code{p.pert} specifies the probability of perturbation for 
\code{type = "independent"}. For example, if \code{p.pert = 1}, each case is
guaranteed to have measurement error on the outcome.

The argument \code{n.pert} specifies the number of perturbations for 
\code{type = "dependent"}. This must be an integer between zero (no case suffers
from measurement error in the outcome or no case gets deleted) and the total 
number of cases in \code{data} (all cases suffer from measurement error in the 
outcome or all cases get deleted.)
}

\references{
Hug, Simon. 2013. \dQuote{Qualitative Comparative Analysis: How Inductive Use and 
Measurement Error lead to Problematic Inference.} \emph{Political Analysis} 21 (2):252-65.
DOI: \href{http://dx.doi.org/10.1093/pan/mps061}{10.1093/pan/mps061}.
}

\seealso{\code{\link{truthTable}}}

\examples{
# replicate results from Hug (2013) for 2 deleted cases
#------------------------------------------------------
(dat <- data.frame(matrix(c(
   rep(1,25),rep(0,20),rep(c(0,0,1,0,0),3),
   0,0,0,1,0,0,1,0,0,0,0,rep(1,7),0,1),
   nrow = 16, byrow = TRUE, dimnames = list(c(
    "AT","DK","FI","NO","SE","AU","CA","FR",
    "US","DE","NL","CH","JP","NZ","IE","BE"),
   c("P", "U", "C", "S", "W"))
)))

retention(dat, outcome = "W", type = "deletion", assump = "dependent", n.pert = 2)
}

\keyword{functions}
