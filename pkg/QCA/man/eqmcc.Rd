\name{eqmcc}

\alias{eqmcc}
\alias{is.qca}

\title{Minimization with Enhanced Quine-McCluskey Algorithm}

\description{
Being the core of the QCA package, this function performs the minimization of 
Boolean or multivalent output functions. It is called "eqmcc" because it is an 
enhancement of the classical Quine-McCluskey minimization algorithm, but modifies
the latter in significant ways.
}

\usage{
eqmcc(data, outcome = c(""), neg.out = FALSE, conditions = c(""), 
      relation = "suf", n.cut = 1, incl.cut1 = 1, incl.cut0 = 1, 
      explain = c("1"), include = c(""), row.dom = FALSE, 
      min.dis = TRUE, omit = c(), dir.exp = c(), details = FALSE,
      show.cases = FALSE, inf.test = c(""), use.tilde = FALSE, 
      use.letters = FALSE, ...)

is.qca(x)
}

\arguments{
  \item{data}{A truth table object or a data set of bivalent crisp-set or
        fuzzy-set variables or multivalent crisp-set variables.}
  \item{outcome}{A vector of outcomes.}
  \item{neg.out}{Logical, use negation of \code{outcome} (ignored if \code{data}
        is a truth table object).}
  \item{conditions}{A vector of condition variables.}
  \item{relation}{The set relation to \code{outcome}, either \code{"suf"} or
        \code{"sufnec"}.}
  \item{n.cut}{The minimum number of cases with set membership score above 0.5 
        for an output function value of "0", "1" or "C".}
  \item{incl.cut1}{The minimum sufficiency inclusion score for an output function 
        value of "1".}
  \item{incl.cut0}{The maximum sufficiency inclusion score for an output function 
        value of "0".}
  \item{explain}{A vector of output function values to be explained.}
  \item{include}{A vector of additional output function values to be included in 
        the minimization.}
  \item{row.dom}{Logical, impose row dominance as constraint on solution to
        eliminate dominated inessential prime implicants.}
  \item{min.dis}{Logical, impose minimal disjunctivity as constraint on solution
        to eliminate models with more prime implicants than the model(s) with
        the fewest prime implicants.}
  \item{omit}{A vector of configuration index values or matrix of configurations 
        to be omitted from minimization.}
  \item{dir.exp}{A vector of directional expectations for deriving intermediate 
        solutions.}
  \item{details}{Logical, present solution details.}
  \item{show.cases}{Logical, also print case names if \code{details = TRUE}.}
  \item{inf.test}{A vector specifying the inference-statistical test to be 
        performed (currently only \code{"binom"}) and the critical significance 
        level.}
  \item{use.tilde}{Logical, use tilde for negation with bivalent variables.}
  \item{use.letters}{Logical, use simple letters instead of original variable 
        names.}
  \item{...}{Other arguments (for backward compatibility).}
  \item{x}{An object of class "qca".}
}

\details{
The argument \code{data} can be a truth table object (an object of class "tt" 
returned by \code{truthTable()}) or a suitable data set. Suitable data sets have 
the following structure: values of 0 and 1 for bivalent crisp-set variables,
values between 0 and 1 for bivalent fuzzy-set variables, and values beginning with
0 at increments of 1 for multivalent crisp-set variables. The placeholder "-"
indicates a "don't care" value in auxiliary condition variables that specify 
temporal order between other substantive condition variables in tQCA. These values 
lead to the exclusion of the auxiliary condition variable from the computation of 
parameters of fit.

The argument \code{outcome} specifies the outcome to be explained, either in
curly-bracket notation (e.g., \code{O{value}}) if the outcome is from a multivalent
or a bivalent variable, or in upper-case notation if the outcome is from a bivalent
variable (e.g., \code{O} to mean \code{O{1}}). Outcomes can be single values of
variables not simultaneously passed to \code{conditions}, or values from any subset
of the variables specified in \code{conditions} if \code{data} is not a truth table
object. At least one outcome always has to be specified.

If multiple outcomes are specified, their variables must also be specified in the
conditions. In this case, truth tables and solution details will not be printed
by default (see the example on mimicing Coincidence Analysis below). Outcomes from
multivalent variables always require curly-bracket notation.

The logical argument \code{neg.out} controls whether \code{outcome} is to be 
explained or its negation. If \code{outcome} is from a multivalent variable, 
\code{neg.out = TRUE} has the effect that the disjunction of all remaining values
becomes the new outcome to be explained. 

The argument \code{conditions} specifies the condition variables. If omitted, 
all variables in \code{data} are used except that for \code{outcome} if there are
not multiple outcomes. With multiple outcomes, all variables in \code{data} are
used. Please note that computation times may increase significantly beyond 17
condition variables and that the compuation of results may not be possible at all
depending on end-user machine constraints.

The argument \code{relation} specifies the relation between the conditions
and the outcome to be analyzed. It accepts either \code{suf} or \code{sufnec}.
If \code{relation = "suf"} (default), sufficiency relations are identified as 
indicated by a right arrow (\code{=>}). If the models identified under a solution
also prove to be necessary, this will be indicated by a double arrow (\code{<=>}).
If \code{relation = "sufnec"}, models must be both sufficient and necessary to
be identified. The argument \code{incl.cut1} then acts as the cut-off for
the sufficiency inclusion of a configuration as well as the necessity inclusion 
of the final model(s).

Configurations that contain fewer than \code{n.cut} cases with membership scores 
above 0.5 are coded as logical remainders (\code{OUT = "?"}). If the number of 
such cases is at least \code{n.cut}, configurations with an inclusion score of at 
least \code{incl.cut1} are coded positive (\code{OUT = "1"}), configurations 
with an inclusion score below \code{incl.cut1} but with at least \code{incl.cut0} 
are coded as a contradiction (\code{OUT = "C"}), and configurations with an 
inclusion score below \code{incl.cut0} are coded negative (\code{OUT = "0"}). 
If \code{incl.cut0} is unspecified, it is set equal to \code{incl.cut1} and no 
contradictions can occur.

The argument \code{explain} specifies a vector of suitable values of the output
function to be minimized. Vectors of such values are \code{"1"}, \code{"C"}, 
\code{"0"}, \code{c("1", "C")} and \code{c("0", "C")}, but not \code{c("1", "0")} 
and \code{c("1", "0", "C")}. Note that for \code{"0"}, \code{"C"} and 
\code{c("0", "C")}, configurations will be reduced but no solution details printed.

The argument \code{include} specifies a vector of suitable values of the output
function to be included in addition to the value(s) specified in \code{explain}. 
All combinations allowed separately for \code{explain} are also allowed for 
\code{include} in combination with \code{explain}.

The logical argument \code{row.dom} controls whether the principle of row dominance
is imposed as a constraint on the solution. An inessential prime implicant \eqn{P}
dominates another \eqn{Q} if all configurations covered by \eqn{Q} are also
covered by \eqn{P}, but they are not interchangeable (cf. McCluskey 1956: 1425;
McCluskey 1965: 164-152). If row dominance is operative, models that contain
dominated prime implicants will not be returned.

The logical argument \code{min.dis} controls whether the principle of minimal
disjunctivity is imposed as a constraint on the solution (McCluskey 1965: 123-126).
If minimal disjunctivity is operative, models that contain more than the number
of prime implicants of the model(s) with the fewest prime implicants will not be
returned. Note that in some cases, the deactivation of both \code{row.dom} and
\code{min.dis} may lead to the identification of so many models that they cannot
be returned. Users should also be aware that \emph{for purposes of causal data
analysis, neither} \code{row.dom} \emph{nor} \code{min.dis} \emph{should be
operative}.

The argument \code{omit} can be used to omit any configuration (positive, 
negative or remainder) from the minimization process. It accepts a vector of row 
numbers from the truth table or a matrix of configurations of the same order of
conditions as passed to truthTable() (if eqmcc() is passed a truth table object) 
or as specified in the argument \code{conditions}.

The \code{dir.exp} argument specifies directional expectations for separating
easy from difficult counterfactuals in simplifying assumptions. For bivalent 
crisp and fuzzy-set variables, expectations should be specified as a vector of
the same length and the same order of condition variables as provided in
\code{conditions}. For bivalent variables, a value of either "0" or "1" indicates
that the corresponding condition is expected to contribute to a positive output
function value, while a dash, "-", indicates that one or the other condition does
so. For multivalent variables, multiple values have to be enclosed by double
quotes and separated by a semicolon (see mvQCA example using Hartmann and Kemmerzell
(2010) below). In some situations, directional expectations in mvQCA generate easy
counterfactuals that do not contribute to parsimony (Thiem 2014). These so-called
non-simplifying easy counterfactuals will not be part of the solution (see mvQCA
example using Sager and Andereggen (2012) below).

If \code{details = TRUE}, paramters of fit (inclusion, raw coverage, and unique 
coverage) will be printed for each solution and its respective prime implicants. 
Essential prime implicants are listed first in the solution output and in the top 
part of the parameters-of-fit table. Inessential prime implicants are listed in 
brackets in the solution output and in the middle part of the parameters-of-fit 
table, together with their unique coverage scores under each individual model.
Inclusion and coverage scores for each model are provided in the bottom part
of the parameters-of-fit table.

The logical argument \code{show.cases} controls whether case names are displayed
next to their corresponding truth table configurations and/or prime implicants 
(do not use with many cases and/or long case names!). In the parameters-of-fit
table, semicolons separate cases from different truth table configurations,
whereas commas separate cases from the same configuration.

The argument \code{inf.test} provides functionality for basing output function
value codings on inference-statistical tests of the observed configurations for 
(bivalent and multivalent) crisp set variables. It requires a vector of length 
two, comprising the test (currently only exact binomial: \code{"binom"}) and a 
significance level. If the empirical inclusion score of a configuration is not 
significantly lower than \code{incl.cut1}, it will be coded positive 
(\code{OUT = "1"}). If it is significantly lower than \code{incl.cut1} but 
significantly higher than \code{incl.cut0}, it will be coded as a contradiction 
(\code{OUT = "C"}). If it is not significantly higher than \code{incl.cut0}, 
it will be coded negative (\code{OUT = "0"}). 

The argument \code{use.tilde} should only be used for bivalent variables. If the
conditions variables have already been named with single letters, the argument
\code{use.letters} will have no effect. Otherwise, it will replace the labels of
the condition variables with single letters in alphabetical order.
}

\value{An object of class "qca" for single outcomes, and "mqca" for multiple
       outcomes. Objects of class "qca" are invisible lists with the following 
       components:\cr
  \item{tt}{The truth table object.}
  \item{excluded}{The line number(s) of the negative configuration(s).}
  \item{initials}{The initial positive configuration(s).}
  \item{PIs}{The prime implicant(s).}
  \item{PIchart}{A list containing the PI chart(s).}
  \item{solution}{A list of solution(s).}
  \item{essential}{A list of essential PI(s).}
  \item{pims}{A list of PI membership scores.}
  \item{SA}{A list of simplifying assumptions.}
  \item{i.sol}{A list of components specific to intermediate solution(s), 
        including the prime implicant chart, prime implicant membership scores, 
        (non-simplifying) easy counterfactuals and difficult counterfactuals.}
}

\references{
Baumgartner, Michael. 2009. Inferring Causal Complexity. 
\emph{Sociological Methods & Research} 38 (1):71-101.

Emmenegger, Patrick. 2011. Job Security Regulations in Western Democracies: A 
Fuzzy Set Analysis. 
\emph{European Journal of Political Research} 50 (3):336-64.

Hartmann, Christof, and Joerg Kemmerzell. 2010. Understanding Variations in Party 
Bans in Africa. 
\emph{Democratization} 17 (4):642-65.

McCluskey, Edward J. 1956. Minimization of Boolean Functions. 
\emph{Bell Systems Technical Journal} 35 (6):1417-44.

McCluskey, Edward J. 1965. \emph{Introduction to the Theory of Switching Circuits}.
Princeton: Princeton University Press.

Krook, Mona Lena. 2010. Women's Representation in Parliament: A Qualitative 
Comparative Analysis. 
\emph{Political Studies} 58 (5):886-908.

Ragin, Charles C., and Sarah Ilene Strand. 2008. Using Qualitative Comparative 
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005). 
\emph{Sociological Methods & Research} 36 (4):431-41.

Thiem, Alrik. 2014. Parameters of Fit and Intermediate Solutions in Multi-Value
Qualitative Comparative Analysis. \emph{Quality & Quantity}
DOI:10.1007/s11135-014-0015-x.
}

\seealso{\code{\link{truthTable}}, \code{\link{factorize}}}

\examples{
# csQCA using Krook (2010)
#-------------------------
data(d.Kro)
head(d.Kro)

# conservative solution
eqmcc(d.Kro, outcome = "WNP")

# negated outcome, conservative solution
eqmcc(d.Kro, outcome = "WNP", neg.out = TRUE)

# parsimonious solution with details and case names
Kro.sp <- eqmcc(d.Kro, outcome = "WNP", include = "?", 
  details = TRUE, show.cases = TRUE)
Kro.sp

# check PI chart
Kro.sp$PIchart

# simplifying assumptions (SAs)
Kro.sp$SA

# minimized expressions for SAs using fake outcome (FO)
for(i in 1:2) {
  print(eqmcc(cbind(Kro.sp$SA[[i]], FO = 1), outcome = "FO"))
}

# conservative solution with truth table object
Kro.tt <- truthTable(d.Kro, outcome = "WNP")
Kro.sc <- eqmcc(Kro.tt)
Kro.sc

# fsQCA using Emmenegger (2011)
#------------------------------
data(d.Emm)
head(d.Emm)

# parsimonious solution with details
eqmcc(d.Emm, outcome = "JSR", incl.cut1 = 0.9, include = "?", 
  details = TRUE)

# intermediate solution
Emm.si <- eqmcc(d.Emm, outcome = "JSR", incl.cut1 = 0.9, 
  include = "?", dir.exp = c(1,1,1,1,1,0), details = TRUE)
Emm.si

# are the prime implicants also sufficient for the negation of the outcome?
pof(Emm.si$i.sol$C1P1$pims, outcome = "JSR", d.Emm, neg.out = TRUE,
  relation = "suf")

# check PI chart for intermediate solution;
# C1P1: first conservative and first parsimonious solution
Emm.si$i.sol$C1P1$PIchart

# same intermediate solution, but not same SAs
identical(rownames(Emm.si$SA$S1), rownames(Emm.si$SA$S2))

# check easy counterfactuals; same
(EC1 <- Emm.si$i.sol$C1P1$EC)
(EC2 <- Emm.si$i.sol$C1P2$EC)
identical(rownames(EC1), rownames(EC2))

# minimized expressions for ECs using fake outcome (FO)
eqmcc(cbind(Emm.si$i.sol$C1P1$EC, FO = 1), outcome = "FO")

# plot all four prime implicants of the intermediate solution
PIsc <- Emm.si$i.sol$C1P1$pims
par(mfrow = c(2, 2))
for(i in 1:4){
 plot(PIsc[, i], d.Emm$JSR, pch = 19, ylab = "JSR",
  xlab = names(PIsc)[i], xlim = c(0, 1), ylim = c(0, 1),
  main = paste("Prime Implicant", print(i)))
 mtext(paste(
  "Inclusion = ", round(Emm.si$i.sol$C1P1$IC$incl.cov$incl[i], 3),
  "; Coverage = ", round(Emm.si$i.sol$C1P1$IC$incl.cov$cov.r[i], 3)), 
  cex = 0.7, line = 0.4)
 abline(h = 0.5, lty = 2, col = gray(0.5))
 abline(v = 0.5, lty = 2, col = gray(0.5))
 abline(0, 1)
}

# mvQCA using Hartmann and Kemmerzell (2010)
#-------------------------------------------
data(d.HK)
head(d.HK)

# create vector of condition variables
conds <- c("C", "F", "T", "V")

# parsimonious solution, with contradictions included
HK.sp <- eqmcc(d.HK, outcome = "PB{1}", conditions = conds,
  incl.cut0 = 0.4, include = c("?", "C"), details = TRUE)
HK.sp

# Venn diagram of solution S1;
# first extract PI membership scores
PIms <- HK.sp$pims

require(VennDiagram)
vennHK.suf <- venn.diagram(
 x = list(
  "PB{1}" = which(d.HK$PB == 1),
  "C{0,1}" = which(PIms[, 1] == 1 | PIms[, 2] == 1),
  "T{2}" = which(PIms[, 4] == 1),
  "T{1}*V{0}" = which(PIms[, 5] == 1)),
 filename = NULL,
 cex = 2.5, cat.cex = 2, cat.pos = c(180, 180, 0, 0),
 cat.dist = c(0.4, 0.4, 0.12, 0.12),
 fill = gray(c(0.3, 0.5, 0.7, 0.9))
)
grid.draw(vennHK.suf)

# which are the two countries in T{2} but not PB{1}?
rownames(d.HK[d.HK$T == 2 & d.HK$PB != 1, ])

# minimize contradictions (only one contradiction)
eqmcc(d.HK, outcome = "PB{1}", conditions = conds, incl.cut0 = 0.4,
  explain = "C")

# intermediate solution with directional expectations:
# C{1}, F{1,2}, T{2}, V contribute to OUT = 1
HK.si <- eqmcc(d.HK, outcome = "PB{1}", conditions = conds,
  include = "?", dir.exp = c(1, "1;2", 2, 1), details = TRUE)
HK.si

# mvQCA using Sager and Andereggen (2012)
#----------------------------------------
data(d.SA)
head(d.SA)

# directional expectation of FED{0} leads to non-simplifying
# easy counterfactual (see Thiem 2014 for more details)
SA.si <- eqmcc(d.SA, outcome = "ACC{1}", conditions = names(d.SA)[1:5],
  include = "?", dir.exp = c(0,1,0,1,1), details = TRUE)
SA.si

SA.si$i.sol$C1P1$NSEC

# tQCA using Ragin and Strand (2008)
#-----------------------------------
data(d.RS)
head(d.RS)

# conservative solution with details and case names;
# auxiliary temporal order condition "EBA" automatically excluded 
# from parameters of fit
eqmcc(d.RS, outcome = "REC", details = TRUE, show.cases = TRUE)

# QCA path models ("causal chain" in CNA); data from Baumgartner (2009);
# note that CNA and QCA results are not always equal because CNA applies a
# different concept of the truth table that does not take each configuration's
# inclusion score into consideration before minimization
#-----------------------------------------------------------------------------
d.Bau <- data.frame(
  U = c(1,1,1,1,0,0,0,0), D = c(1,1,0,0,1,1,0,0),
  L = c(1,1,1,1,1,1,0,0), G = c(1,0,1,0,1,0,1,0),
  E = c(1,1,1,1,1,1,1,0),
  row.names = letters[1:8])
head(d.Bau)

# with multiple outcomes, no solution details are printed;
# "causal-chain structure": (D + U <=> L) * (G + L <=> E)
# "common-cause structure": (D + U <=> L) * (G + D + U <=> E)
Bau.cna <- eqmcc(d.Bau, outcome = names(d.Bau), relation = "sufnec", 
  include = "?", min.dis = FALSE)
Bau.cna

# get the truth table, solution details and case names for outcome "E"
print(Bau.cna$E, details = TRUE, show.cases = TRUE)

# QCA with multiple outcomes from multivalent variables
#------------------------------------------------------
d.mmv <- data.frame(A = c(2,0,0,1,1,1,2,2), B = c(2,2,2,2,1,1,0,0), 
                    C = c(0,1,0,0,0,2,1,0), D = c(2,1,2,2,3,1,3,0), 
                    E = c(3,2,3,3,0,1,3,2), 
  row.names = letters[1:8])
head(d.mmv)

mmv.s <- eqmcc(d.mmv, outcome = c("D{2}", "E{3}"))
mmv.s

# use quotes with curly-bracket notation to access solution component
print(mmv.s$"E{3}", details = TRUE, show.cases = TRUE)

# negation of outcome from multivalent variable is disjunction of all other
# values; high level of ambiguity (18 models)
mmv.s <- eqmcc(d.mmv, outcome = "E{3}", neg.out = TRUE, include = "?",
 min.dis = FALSE)
mmv.s

}

\keyword{functions}


