\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
QCA: A Package for Qualitative Comparative Analysis
}

\description{
The \pkg{QCA} package implements the method of \dfn{Qualitative Comparative Analysis}
(QCA) as developed by Ragin (1987), and extended by Ragin (2000, 2008), Cronqvist
and Berg-Schlosser (2009). QCA is a family of configurational comparative
techniques that use Boolean algebra and its generalizations by fusing concepts
from set theory and propositional logic.

Three variants can currently be processed by \pkg{QCA}: \dfn{crisp-set QCA} (csQCA),
\dfn{multi-value QCA} (mvQCA) and \dfn{fuzzy-set QCA} (fsQCA). A subvariant of
csQCA called \dfn{temporal QCA} (tQCA) is also available (Caren and Panofsky 2005;
Ragin and Strand 2008). The \pkg{QCA} package extends these basic variants in 
different directions by implementing complementary functionality. For example,
intermediate solutions and summary statistics are also available in mvQCA (Thiem 
2014a). Note that functionality for gsQCA (Thiem, 2014b) is in development but 
not yet implemented.

As of version 0.4-5, \pkg{QCA} has offered a new function called \code{\link{eqmcc}}
(\dfn{enhanced Quine-McCluskey}; \emph{e}QMC) that finds exact solutions much 
faster and with substantially lower memory consumption than classical Quine-McCluskey 
optimization (Dusa 2007, 2010). Since version 1.0-4, the compilation of major parts 
of the code in the C language has increased the speed of some core functions such 
as \code{\link{superSubset}} considerably. Since version 1.1-0, \code{\link{eqmcc}} 
has also been made more flexible: \dfn{row dominance} and \dfn{minimal disjunctivity} 
can be imposed as optional solution constraints (Thiem and Dusa 2013a), QCA 
\dfn{path models} can be identified (equivalent of \dfn{causal chains} in Coincidence 
Analysis (Baumgartner 2009a, b)), multiple outcomes even from multivalent variables 
are allowed, and truth tables for csQCA and mvQCA can now be coded on the basis 
of inference-statistical tests (currently only exact binomial). For more details,
see the package's change log file.

Several data sets from various areas of the social science are integrated in 
\pkg{QCA} so as to facilitate users' familiarization with the package's functionality. 
Areas currently covered include business, management & organization (\code{\link{d.stakeholder}}),
education (\code{\link{d.education}}), environmental sciences (\code{\link{d.biodiversity}}),
evaluation (\code{\link{d.transport}}), legal studies (\code{\link{d.napoleon}}), 
political science (\code{\link{d.jobsecurity}}, \code{\link{d.partybans}}, \code{\link{d.represent}}), 
public health (\code{\link{d.health}}), urban affairs (\code{\link{d.urban}}), 
and sociology (\code{\link{d.homeless}}, \code{\link{d.socialsecurity}}). For more 
details, see the data sets' documentation files. Please also note that many more 
data sets for QCA in both raw and calibrated format are available on the COMPASSS 
website at \url{http://www.compasss.org/bibdata.htm}.

How to make the most of the package's capabilities is demonstrated in detail by 
Thiem and Dusa (2013c) with examples from political science (note that data sets 
have been renamed). Later versions of the package than those used in Thiem and 
Dusa (2013b, 2013c) may feature slightly different functions, arguments, values 
and default settings, but backward compatibility is generally ensured. A general
comparison of \pkg{QCA}'s features with those of \pkg{fs/QCA} (Ragin and Davey 
2014) and \pkg{Tosmana} (Cronqvist 2011) is provided by Thiem and Dusa (2013a).

If you make use of the \pkg{QCA} package in your work, please acknowledge this, 
not least for reasons of scientific transparency. The software citation for \pkg{QCA} 
loads on attaching the package or by using the command \code{citation("QCA")} after
having loaded the package. The aforesaid command also provides a suitable BibTeX 
entry. In addition, please either cite Thiem and Dusa (2013b) or/and Thiem and 
Dusa (2013c).
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 1.1-3.3\cr
    Date:    \tab 2014-09-09\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

Alrik Thiem\cr
Department of Philosophy\cr
University of Geneva\cr
\email{alrik.thiem@unige.ch}

\bold{Maintainer}:\cr
Adrian Dusa
}

\references{
Baumgartner, Michael. 2009a. \dQuote{Uncovering Deterministic Causal Structures: 
A Boolean Approach.} \emph{Synthese} 170 (1):71-96. 
DOI: \href{http://dx.doi.org/10.1007/s11229-008-9348-0}{10.1007/s11229-008-9348-0}.

Baumgartner, Michael. 2009b. \dQuote{Inferring Causal Complexity.} 
\emph{Sociological Methods & Research} 38 (1):71-101. 
DOI: \href{http://dx.doi.org/10.1177/0049124109339369}{10.1177/0049124109339369}.

Caren, Neal, and Aaron Panofsky. 2005. \dQuote{TQCA: A Technique for Adding 
Temporality to Qualitative Comparative Analysis.} 
\emph{Sociological Methods & Research} 34 (2):147-72.
DOI: \href{http://dx.doi.org/10.1177/0049124105277197}{10.1177/0049124105277197}.

Cronqvist, Lasse. 2011. \emph{Tosmana: Tool for Small-N Analysis, Version 1.3.2.0 
[Computer Program]}. Trier: University of Trier.

Cronqvist, Lasse, and Dirk Berg-Schlosser. 2009. \dQuote{Multi-Value QCA (mvQCA).} 
In \emph{Configurational Comparative Methods: Qualitative Comparative Analysis (QCA) 
and Related Techniques}, eds. B. Rihoux and C. C. Ragin. London: Sage Publications, 
pp. 69-86.

Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: Working Paper 
2007-49. URL: \url{http://www.compasss.org/wpseries/Dusa2007b.pdf}.

Dusa, Adrian. 2010. \dQuote{A Mathematical Approach to the Boolean Minimization 
Problem.} \emph{Quality & Quantity} 44 (1):99-113.
DOI: \href{http://dx.doi.org/10.1007/s11135-008-9183-x}{10.1007/s11135-008-9183-x}.

Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Ragin, Charles C. 2000. \emph{Fuzzy-Set Social Science}. Chicago: University of 
Chicago Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
Chicago: University of Chicago Press.

Ragin, Charles C., and Sean Davey. 2014. \emph{fs/QCA: Fuzzy-Set/Qualitative 
Comparative Analysis, Version 2.5 [Computer Program]}. 
Irvine: Department of Sociology, University of California.

Ragin, Charles C., and Sarah Ilene Strand. 2008. \dQuote{Using Qualitative 
Comparative Analysis to Study Causal Order: Comment on Caren and Panofsky (2005).} 
\emph{Sociological Methods & Research} 36 (4):431-41.
DOI: \href{http://dx.doi.org/10.1177/0049124107313903}{10.1177/0049124107313903}.

Thiem, Alrik. 2014a. \dQuote{Parameters of Fit and Intermediate Solutions in 
Multi-Value Qualitative Comparative Analysis.} \emph{Quality & Quantity}.
DOI: \href{http://dx.doi.org/10.1007/s11135-014-0015-x}{10.1007/s11135-014-0015-x}.

Thiem, Alrik. 2014b. \dQuote{Unifying Configurational Comparative Methods: 
Generalized-Set Qualitative Comparative Analysis.} \emph{Sociological Methods & 
Research} 43 (2):313-37.
DOI: \href{http://dx.doi.org/10.1177/0049124113500481}{10.1177/0049124113500481}.

Thiem, Alrik, and Adrian Dusa. 2013a. \dQuote{Boolean Minimization in Social 
Science Research: A Review of Current Software for Qualitative Comparative 
Analysis (QCA).} \emph{Social Science Computer Review} 31 (4):505-21.
DOI: \href{http://dx.doi.org/10.1177/0894439313478999}{10.1177/0894439313478999}.

Thiem, Alrik, and Adrian Dusa. 2013b. \dQuote{QCA: A Package for Qualitative 
Comparative Analysis.} \emph{The R Journal} 5 (1):87-97. 
URL: \url{http://journal.r-project.org/archive/2013-1/thiem-dusa.pdf}.

Thiem, Alrik, and Adrian Dusa. 2013c. \emph{Qualitative Comparative Analysis with R:
A User's Guide}. New York: Springer.
}

\keyword{package}

