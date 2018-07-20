\name{minimize}

\alias{minimize}
\alias{eqmcc}

\title{Minimize a truth table}

\description{
This function performs the QCA minimization of an input truth table, or if the
input is a dataset the minimization it minimizes a set of causal conditions with
respect to an outcome. Three minimization methods are available: the classical
Quine-McCluskey, the enhanced Quine-McCluskey and the latest Consistency Cubes
algorithm that is built for performance.

All algorithms return the same, exact solutions, see
\href{http://dx.doi.org/10.13140/RG.2.2.36359.19361/4}{Dusa (2017)}
and \href{http://dx.doi.org/10.1080/0022250X.2014.897949}{Dusa and Thiem (2015)}.
}

\usage{
minimize(input, include = "", exclude = NULL, dir.exp = "",
         pi.cons = 0, pi.depth = 0, sol.cons = 0, sol.cov = 1, sol.depth = 0,
         min.pin = FALSE, row.dom = FALSE, all.sol = FALSE,
         details = FALSE, use.tilde = FALSE, method = "CCubes", ...)
}
      
\arguments{
  \item{input}{A truth table object (preferred) or a data frame containing 
        calibrated causal conditions and an outcome.}
  \item{include}{A vector of other output values to include in the minimization
        process.}
  \item{exclude}{A vector of row numbers from the truth table, or a matrix of causal 
        combinations to exclude from the minimization process.}
  \item{dir.exp}{A vector of directional expectations to derive intermediate
        solutions.}
  \item{pi.cons}{Numerical fuzzy value between 0 and 1, minimal consistency
        threshold for a prime implicant to be declared as sufficient.}
  \item{pi.depth}{Integer, a maximum number of causal conditions to be used
        when searching for conjunctive prime implicants.}
  \item{sol.cons}{Numerical fuzzy value between 0 and 1, minimal consistency 
        threshold for a model to be declared as sufficient.}
  \item{sol.cov}{Numerical fuzzy value between 0 and 1, minimal coverage 
        threshold for a model to be declared as necessary.}
  \item{sol.depth}{Integer, a maximum number of prime implicants to be used when
        searching for disjunctive solutions.}
  \item{min.pin}{Logical, terminate the search at the depth where newly found
        prime implicants do not contribute to minimally solving the PI chart.}
  \item{row.dom}{Logical, perform row dominance in the prime implicants' chart to
        eliminate redundant prime implicants.}
  \item{all.sol}{Logical, search for all possible solutions even of not minimal.}
  \item{details}{Logical, print more details about the solution.}
  \item{use.tilde}{Logical, use tilde to signal the absence of conditions.}
  \item{method}{Minimization method, one of "CCubes" (default), or "QMC" the
        classical Quine-McCluskey, or "eQMC" the enhanced Quine-McCluskey.}
  \item{...}{Other arguments to be passed to function \code{\link{truthTable}}.}
}

\details{
Most of the times, this function takes a truth table object as the \bold{\code{input}} 
for the minimization procedure, but the same argument can refer to a data frame
containing calibrated columns.

For the later case, the function \bold{\code{minimize()}} originally had some additional
formal arguments which were sent to the function \bold{\code{truthTable()}}:
\bold{\code{outcome}}, \bold{\code{conditions}}, \bold{\code{n.cut}},
\bold{\code{incl.cut}}, \bold{\code{show.cases}}, \bold{\code{use.letters}} and
\bold{\code{inf.test}}.

All of these parameters are still possible with function \bold{\code{minimize()}}, but
since they are sent to the \bold{\code{truthTable()}} function anyway, it is
unnecessary to duplicate their explanation here. The only situation which does need
an additional description relates to the argument \bold{\code{outcome}}, where
unlike \bold{\code{truthTable()}} which accepts a single one, the function
\bold{\code{minimize()}} accepts multiple outcomes and performs a minimization for 
each of them (a situation when all columns are considered causal conditions).

The argument \bold{\code{include}} specifies which other truth table rows are
included in the minimization process. Most often, the remainders are included but
any value accepted in the argument \bold{\code{explain}} is also accepted in the
argument \bold{\code{include}}.

The argument \bold{\code{exclude}} is used to exclude truth table rows from the
minimization process, from the positive configurations and/or from the remainders.
It can be specified as a vector of truth table line numbers, or as a matrix of
causal combinations.

The argument \bold{\code{dir.exp}} is used to specify directional expectations, as
described by Ragin (2003). They can be specified as a single string, with values
separated by commas. For multi-value directional expectations, they are specified
together, separated by semicolons. The total length of the directional expectations
must match the number of causal conditions specified in the analysis, using a dash
\bold{\code{"-"}} if there are no particular expectations for a specific causal
condition.

Activating the \bold{\code{details}} argument has the effect of printing parameters
of fit for each prime implicant and each overall solution, the essential prime
implicants being listed in the top part of the table. It also prints the truth
table, in case the argument \bold{\code{input}} has been provided as a data frame
instead of a truth table object.

The argument \bold{\code{use.tilde}} signals the absence of a causal condition, in
a sufficiency relation with the outcome, using a tilde sign \bold{\code{"~"}}. It
is ignored if the data is multivalent.

By default, the package \pkg{QCA} employes a different search algorithm based on Consistency
Cubes (Dusa, 2017), analysing all possible combinations of causal conditions and all
possible combinations of their respective levels. The structure of the input dataset
(number of causal conditions, number of levels, number of unique rows in the truth
table) has a direct implication on the search time, as all of those characteristics
become entry parameters when calculating all possible combinations.

Consequently, two kinds of depth arguments are provided: 

\tabular{rl}{
\bold{\code{pi.depth}} \tab the maximum number of causal conditions needed to construct a prime\cr
 \tab implicant, the complexity level where the search can be stopped, as long as the\cr
 \tab PI chart can be solved.\cr
\bold{\code{sol.depth}} \tab the maximum number of prime implicants needed to find a solution\cr
 \tab (to cover all initial positive output configurations)
}



These arguments introduce a possible new way of deriving prime implicants and solutions,
that can lead to different results (i.e. even more parsimonious) compared to the classical
Quine-McCluskey. When either of them is modified from the default value of 0, the minimization
method is automatically set to \bold{\code{"CCubes"}} and the remainders are automatically
included in the minimization.

The search time is larger the higher these depths, or inversely the search time can be
significantly shorter if these depths are smaller. Irrespective of how large 
\bold{\code{pi.depth}} is, the algorithm will always stop at a maximum complexity level
where no new, non-redundant prime implicants are found. The argument \bold{\code{sol.depth}}
is relevant only when activating the argument \bold{\code{all.sol}} to solve the PI chart.

The default method (when \bold{\code{all.sol = FALSE}}), is to find
the minimal number (\bold{\code{k}}) of prime implicants needed to cover all initial
positive output configurations, then it exhaustively searches through all possible
disjunctions of \bold{\code{k}} prime implicants which do cover those configurations.

The argument \bold{\code{min.pin}} introduces an additional
parameter to control when to stop the search for prime implicants. It is based on the
observation by \href{http://dx.doi.org/10.13140/RG.2.2.36359.19361/4}{Dusa (2017)} that
out of the entire set of non redundant prime implicants, only a subset actually contribute
to solving the chart with disjunctions of \bold{\code{k}} PIs. The search depth can be
shortened at the level where the next subset of PIs do not contribute to solving the
PI chart, thus avoiding to spend unnecessary time on finding the maximal number of
non-redundant PIs. Instead, it finds the set of minimal ("min") number of PIs ("pin")
necessary to obtain exactly the same solutions, with a dramatically improved overall
performance.

Once the PI chart is constructed using the prime implicants found in the previous
stages, the argument \bold{\code{row.dom}} can be used to further eliminate
irrelevant prime implicants when solving the PI chart, applying the principle of row
dominance: if a prime implicant A covers the same (intial) positive output
configurations as another prime implicant B and in the same time covers
other configurations which B does not cover, then B is irrelevant and eliminated.

The argument \bold{\code{all.sol}} automatically deactivates the argument
\bold{\code{min.pin}}, because it aims to exhaustively identify all possible
non-redundant disjunctions of \bold{\code{n}} prime implicants that solve the PI
chart, where \bold{\code{n >= k}}, with an inflated number of possible solutions.
Depending on the complexity of the PI chart, sometimes it may take a very long time
to identify all possible non-redundant (disjunctions that are not subsets of
previously found) disjunctive solutions. In such a situation, the number of
combinations of all possible numbers of prime implicants is potentially too large to be
solved in a polynomial time and if not otherwise specified, the depth for the disjunctive
solutions is automatically bounded to 5 prime implicants.

The task of solving the PI chart depends on its size, with prime implicants on the
rows and the positive output configurations on the columns. Since the columns are
fixed, another possible way to reduce the solving time is to eliminate redundant rows,
by activating the argument \bold{\code{row.dom}}.

If minimizing a dataset instead of a truth table, unless otherwise specified the 
argument \bold{\code{incl.cut}} is automatically set to the minimum value between
\bold{\code{pi.cons}} and \bold{\code{sol.cons}}, then passed to the function
\bold{\code{truthTable()}}.

The argument \bold{\code{sol.cons}} introduces another possibility to change the method of
solving the PI chart. Normally, once the solutions are found among all possible combinations
of \bold{\code{k}} prime implicants, consistencies and coverages are subsequently calculated.
When \bold{\code{sol.cons}} is lower than 1, then solutions are searched based on their
consistencies, which should be at least equal to this threshold.
}

\value{
An object of class \code{"qca"} when using a single outcomes, or class \code{"mqca"}
when using multiple outcomes. These objects are lists having the following components:

\item{tt}{The truth table object.}
\item{options}{Values for the various options used in the function (including defaults).}
\item{negatives}{The line number(s) of the negative configuration(s).}
\item{initials}{The initial positive configuration(s).}
\item{PIchart}{A list containing the PI chart(s).}
\item{primes}{The prime implicant(s).}
\item{solution}{A list of solution(s).}
\item{essential}{A list of essential PI(s).}
\item{pims}{A list of PI membership scores.}
\item{IC}{The matrix containing the inclusion and coverage scores for the solution(s).}
\item{SA}{A list of simplifying assumptions.}
\item{i.sol}{A list of components specific to intermediate solution(s), each having
      a PI chart, prime implicant membership scores, (non-simplifying) easy
      counterfactuals and difficult counterfactuals.}
\item{call}{The user's command which produced all these objects and result(s).}
}

\author{
Adrian Dusa
}

\references{
Cebotari, V.; Vink, M.P. (2013) \dQuote{A Configurational Analysis of Ethnic
Protest in Europe}. \emph{International Journal of Comparative Sociology}
vol.54, no.4, pp.298-324.

Cebotari, V.; Vink, M.P. (2015) \dQuote{Replication Data for: A configurational
analysis of ethnic protest in Europe}, Harvard Dataverse, V2, DOI:
\href{http://dx.doi.org/10.7910/DVN/PT2IB9}{10.7910/DVN/PT2IB9},


Cronqvist, L.; Berg-Schlosser, D. (2009) \dQuote{Multi-Value QCA (mvQCA)}, in
Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods. Qualitative
Comparative Analysis (QCA) and Related Techniques}, SAGE.

Dusa, A.; Thiem, A. (2015) \dQuote{Enhancing the Minimization of Boolean and
Multivalue Output Functions With eQMC} \emph{Journal of Mathematical Sociology}
vol.39, no.2, pp.92-108,\cr
DOI: \href{http://dx.doi.org/10.1080/0022250X.2014.897949}{10.1080/0022250X.2014.897949}.

Dusa, A. (2017) \dQuote{Consistency Cubes: A Fast, Efficient Method for Boolean
Minimization}, unpublished working paper on
\href{https://www.researchgate.net/profile/Adrian_Dusa}{ResearchGate}, DOI:
\href{http://dx.doi.org/10.13140/RG.2.2.36359.19361/1}{RG.2.2.36359.19361/1}

Ragin, C. (2003) \emph{Recent Advances in Fuzzy-Set Methods and Their Application to Policy Questions}.
WP 2003-9, \href{http://www.compasss.org/wpseries/Ragin2003a.pdf}{COMPASSS}.

Ragin, C. (2009) \dQuote{Qualitative Comparative Analysis Using Fuzzy-Sets (fsQCA)},
in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.

Ragin, C.C.; Strand, S.I. (2008) \dQuote{Using Qualitative Comparative 
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005).} 
\emph{Sociological Methods & Research} vol.36, no.4, pp.431-441.

Rihoux, B.; De Meur, G. (2009) \dQuote{Crisp Sets Qualitative Comparative Analysis
(mvQCA)}, in Rihoux, B.; Ragin, C. (eds.) \emph{Configurational Comparative Methods.
Qualitative Comparative Analysis (QCA) and Related Techniques}, SAGE.

}

\seealso{\code{\link{truthTable}}, \code{\link{factorize}}}

\examples{

    
# -----
# Lipset binary crisp data
data(LC)

# the associated truth table
ttLC <- truthTable(LC, "SURV", sort.by = "incl, n", show.cases = TRUE)
ttLC

# conservative solution (Rihoux & De Meur 2009, p.57)
cLC <- minimize(ttLC)
cLC

# view the Venn diagram for the associated truth table
library(venn)
venn(cLC)

# add details and case names
minimize(ttLC, details = TRUE)

# negating the outcome
ttLCn <- truthTable(LC, "~SURV", sort.by = "incl, n", show.cases = TRUE)
minimize(ttLCn)

# using a tilde instead of upper/lower case names
minimize(ttLCn, use.tilde = TRUE)

# parsimonious solution, positive output
pLC <- minimize(ttLC, include = "?", details = TRUE)
pLC

# the associated simplifying assumptions
pLC$SA

# parsimonious solution, negative output
pLCn <- minimize(ttLCn, include = "?", details = TRUE)
pLCn


# -----
# Lipset multi-value crisp data (Cronqvist & Berg-Schlosser 2009, p.80)
data(LM)

# truth table 
ttLM <- truthTable(LM, "SURV", conditions = "DEV, URB, LIT, IND",
        sort.by = "incl", show.cases = TRUE)

# conservative solution, positive output
minimize(ttLM, details = TRUE)

# parsimonious solution, positive output
minimize(ttLM, include = "?", details = TRUE)

# negate the outcome
ttLMn <- truthTable(LM, "~SURV", conditions = "DEV, URB, LIT, IND",
         sort.by = "incl", show.cases = TRUE)

# conservative solution, negative output
minimize(ttLMn, details = TRUE)

# parsimonious solution, positive output
minimize(ttLMn, include = "?", details = TRUE)

# -----
# Lipset fuzzy sets data (Ragin 2009, p.112)
data(LF)
# truth table using a very low inclusion cutoff
ttLF <- truthTable(LF, "SURV", incl.cut = 0.7,
        sort.by = "incl", show.cases = TRUE)

# conservative solution
minimize(ttLF, details = TRUE)

# parsimonious solution
minimize(ttLF, include = "?", details = TRUE)

# intermediate solution using directional expectations
iLF <- minimize(ttLF, include = "?", details = TRUE,
                dir.exp = "1,1,1,1,1")
iLF



# -----
# Cebotari & Vink (2013, 2015)
data(CVF) 

ttCVF <- truthTable(CVF, outcome = "PROTEST", incl.cut = 0.8,
                    sort.by = "incl, n", show.cases = TRUE)

pCVF <- minimize(ttCVF, include = "?", details = TRUE)
pCVF

# inspect the PI chart
pCVF$PIchart

# DEMOC*ETHFRACT*poldis is dominated by DEMOC*ETHFRACT*GEOCON
# using row dominance to solve the PI chart
pCVFrd <- minimize(ttCVF, include = "?", row.dom = TRUE, details = TRUE)

# plot the prime implicants on the outcome
pims <- pCVFrd$pims

par(mfrow = c(2, 2))
for(i in 1:4) {
    XYplot(pims[, i], CVF$PROTEST, cex.axis = 0.6)
}



# -----
# temporal QCA (Ragin & Strand 2008) serving the input as a dataset,
# which will automatically be passed to truthTable() as an intermediary
# step before the minimization

data(RS)
minimize(RS, outcome = "REC", details = TRUE)

}

\keyword{functions}


