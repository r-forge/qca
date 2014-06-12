\name{superSubset}
\alias{superSubset}

\title{Find Superset and Subset Relations}

\description{
This function finds all combinations of conditions among all possible combinations 
that optimize the fulfilment of the specified criteria for a superset (necessity)
or subset (sufficiency) relation to the outcome with respect to their complexity.
}

\usage{
superSubset(data, outcome = "", neg.out = FALSE, conditions = c(""), 
            relation = "nec", incl.cut = 1, cov.cut = 0, 
            use.tilde = FALSE, use.letters = FALSE, ...)
}

\arguments{
  \item{data}{A data set of bivalent crisp-set or fuzzy-set variables or multivalent
        crisp-set variables.}
  \item{outcome}{The name of the outcome.}
  \item{neg.out}{Logical, use negation of \code{outcome}.}
  \item{conditions}{A vector of condition variables.}
  \item{relation}{The set relation to \code{outcome}, either \code{"nec"},
        \code{"suf"}, \code{"necsuf"} or \code{"sufnec"}.}
  \item{incl.cut}{The minimal inclusion score of the set relation.}
  \item{cov.cut}{The minimal coverage score of the set relation.}
  \item{use.tilde}{Logical, use tilde for negation with bivalent variables.}
  \item{use.letters}{Logical, use simple letters instead of original variable
        names.}
  \item{...}{Other arguments for backward compatibility.}
}

\details{
This function returns a list of those of the 
\eqn{\prod_{j = 1}^{k}{(p_{j} + 1) - 1}}{\prod_{j = 1}^{k}{(p_{j} + 1) - 1}} 
potential value combinations, where \eqn{p_{j}}{p_{j}} is the number of values 
for condition variable \eqn{j}{j} and \eqn{k}{k} is the number of condition 
variables, that define minimal condition sets for the specified inclusion 
(consistency) and coverage score cut-offs with respect to an outcome. The 
following paragraphs describe the theoretical idea.

If \code{relation = "nec"} (default), the function finds (combinations of) 
conditions that are supersets of (necessary for) the outcome. It starts with an 
initiation set, which is comprised of all \eqn{\sum_{j = 1}^{k}{p_{j}}} simple 
condition sets. This set is expanded by incrementally forming set-theoretic 
intersections of a higher order as long as \code{incl.cut} and \code{cov.cut} 
are still met (the former always takes precedence over the latter). If suitable
intersections exist, they will be returned, together with all their lower-order
components.

If none of the simple conditions or their negations in the initiation set passes 
\code{incl.cut}, set-theoretic unions instead of intersections are formed until 
\code{incl.cut} and \code{cov.cut} will have been met. Only the unions thus found 
will be returned, but not their components. Note that this approach for finding
minimally necessary combinations differs from the one suggested by Bol and Luppi
(2013).

If \code{relation = "suf"}, the function finds (combinations of) 
conditions that are subsets of (sufficient for) the outcome. The initiation set 
is comprised of all \eqn{\prod_{j = 1}^{k}{p_{j}}}{\prod_{j = 1}^{k}{p_{j}}} 
intersections of order \eqn{k}{k}. This set is reduced by incrementally forming 
intersections of a lower order as long as \code{incl.cut} and \code{cov.cut} are 
still met. Only the intersections of the lowest order will be printed. For more 
details, see Thiem and Dusa (2013). For \code{relation = "necsuf"} and 
\code{relation = "sufnec"}, \code{incl.cut} will be applied to each relation and 
\code{cov.cut} has no effect.

The argument \code{outcome} specifies the outcome to be explained. Outcomes from 
multivalent variables require curly-bracket notation (\code{X{value}}).

The logical argument \code{neg.out} controls whether \code{outcome} is to be 
explained or its negation. If \code{outcome} is from a multivalent variable, 
\code{neg.out = TRUE} has the effect that the union of all remaining values 
becomes the new outcome to be explained.

The argument \code{conditions} specifies the condition variables. If omitted, 
all variables in \code{data} are used except \code{outcome}.

The argument \code{use.tilde} only applies to bivalent set variables. If the 
conditions are already named with single letters, the argument \code{use.letters} 
has no effect.

For backward compatibility, the logical argument \code{PRI} returns PRI 
(proportional reduction in inconsistency) scores (see Schneider and Wagemann 
2012, pp. 241-249; Thiem and Dusa 2013, pp. 63-64, 78-79). However, note that 
direct tests on negated outcomes and/or conditions should be preferred.
}

\value{An object of class "ss", which is a list with the following components:\cr
  \item{incl.cov}{A data frame with the parameters of fit.}
  \item{coms}{A data frame with the combination membership scores.}
}
    
\references{
Bol, Damien, and Francesca Luppi. 2013. Confronting Theories Based on Necessary 
Relations: Making the Best of QCA Possibilities. 
\emph{Political Research Quarterly} 66 (1):205-10.

Emmenegger, Patrick. 2011. Job Security Regulations in Western Democracies: A 
Fuzzy Set Analysis. 
\emph{European Journal of Political Research} 50 (3):336-64.

Hartmann, Christof, and Joerg Kemmerzell. 2010. Understanding Variations in Party 
Bans in Africa. 
\emph{Democratization} 17 (4):642-65.

Krook, Mona Lena. 2010. Women's Representation in Parliament: A Qualitative 
Comparative Analysis. 
\emph{Political Studies} 58 (5):886-908.

Schneider, Carsten Q., and Claudius Wagemann. 2012. \emph{Set-Theoretic Methods 
for the Social Sciences: A Guide to Qualitative Comparative Analysis (QCA)}. 
Cambridge: Cambridge University Press.

Thiem, Alrik, and Adrian Dusa. 2013. \emph{Qualitative Comparative Analysis with R: 
A User's Guide}. New York: Springer.   
}

\seealso{\code{\link{pof}}}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(d.Kro)
head(d.Kro)

# find all minimally necessary combinations with an inclusion score 
# of at least 0.9 and coverage score of at least 0.65 
Kro.ss <- superSubset(d.Kro, outcome = "WNP", incl.cut = 0.9, 
  cov.cut = 0.65)
Kro.ss

# Venn diagram of combinations;
# first extract membership scores
head(Kro.coms <- Kro.ss$coms)

require(VennDiagram)
vn.Kro.nec <- venn.diagram(
 x = list(
  "WNP" = which(d.Kro$WNP == 1),
  "wm+LP" = which(Kro.coms[, 1] == 1),
  "WS+LP" = which(Kro.coms[, 2] == 1),
  "ES+LP" = which(Kro.coms[, 3] == 1)),
 filename = NULL,
 cex = 2.5, cat.cex = 2, cat.pos = c(-10, 10, 0, 0),
 cat.dist = c(0.25, 0.25, 0.12, 0.12),
 fill = gray(c(0.3, 0.5, 0.7, 0.9))
)
grid.draw(vn.Kro.nec)

# mvQCA using Hartmann and Kemmerzell (2010)
#-------------------------------------------
data(d.HK)
head(d.HK)

# find all minimally necessary combinations with perfect inclusion
HK.ss <- superSubset(d.HK, outcome = "PB", 
  conditions = c("C", "F", "T", "V"))
HK.ss

# combination membership scores for all cases (only first ten lines)
HK.ss$coms[1:10, , drop = FALSE]

# fsQCA using Emmenegger (2011)
#------------------------------
data(d.Emm)
head(d.Emm)

# find all minimally sufficient combinations with an inclusion score 
# of at least 0.9 and coverage score of at least 0.4; also return 
# PRI (proportional reduction in inconsistency) scores 
Emm.ss <- superSubset(d.Emm, outcome = "JSR", relation = "suf", 
  incl.cut = 0.9, cov.cut = 0.4, PRI = TRUE)
Emm.ss

# same criteria, but for the negation of the outcome
Emm.ss.n <- superSubset(d.Emm, outcome = "JSR", neg.out = TRUE, 
  relation = "suf", incl.cut = 0.9, cov.cut = 0.4, use.tilde = TRUE)
Emm.ss.n

# plot all four combinations
head(Emm.coms <- Emm.ss.n$coms)
par(mfrow = c(2, 2))
for(i in 1:4){
 plot(Emm.coms[, i], 1 - d.Emm$JSR, pch = 19, ylab = "~JSR",
  xlab = names(Emm.coms)[i], xlim = c(0, 1), ylim = c(0, 1),
  main = paste("Combination", print(i)))
 mtext(paste(
  "Inclusion = ", round(Emm.ss.n$incl.cov$incl[i], 3),
  "; Coverage = ", round(Emm.ss.n$incl.cov$cov[i], 3)), 
  cex = 0.7, line = 0.4)
 abline(h = 0.5, lty = 2, col = gray(0.5))
 abline(v = 0.5, lty = 2, col = gray(0.5))
 abline(0, 1)
}

}

\keyword{functions}
