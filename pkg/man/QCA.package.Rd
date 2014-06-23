\name{QCA-package}

\alias{QCA-package}

\alias{QCA}

\docType{package}

\title{
A Package for Qualitative Comparative Analysis
}

\description{
The \pkg{QCA} package implements the method of \dfn{Qualitative Comparative Analysis}
(QCA) as developed by Ragin (1987), and extended by Ragin (2000, 2008), Cronqvist
and Berg-Schlosser (2009). QCA is a family of configurational comparative
techniques that use Boolean algebra and its generalizations by fusing concepts
from set theory and propositional logic.

Three variants can currently be processed: \dfn{crisp-set QCA} (csQCA),
\dfn{multi-value QCA} (mvQCA) and \dfn{fuzzy-set QCA} (fsQCA). A subvariant of
csQCA called \dfn{temporal QCA} (tQCA) is also available (Caren and Panofsky 2005;
Ragin and Strand 2008). Furthermore, the \pkg{QCA} package extends these basic
variants in different directions by implementing complementary functionality.

As of version 0.4-5, \pkg{QCA} has got a new function called \code{\link{eqmcc}}
(\dfn{enhanced Quine-McCluskey}; QMC) that finds exact solutions much faster and
with substantially lower memory consumption than QMC (Dusa 2007, 2010). Since
version 1.0-4, the compilation of major parts of the code in the C language has
increased the speed of some core functions such as \code{\link{superSubset}}
considerably. Since version 1.1-0, \code{\link{eqmcc}} has also been made more
flexible: \dfn{row dominance} and \dfn{minimal disjunctivity} can be imposed as
solution constraints (Thiem and Dusa 2013a), QCA \dfn{path models} can be
identified (equivalent of \dfn{causal chains} in Coincidence Analysis
(Baumgartner 2009a, b)), multiple outcomes even from multivalent variables are
allowed, and truth tables for csQCA and mvQCA can now be coded on the basis of
inference-statistical tests (currently only exact binomial). For more details,
see the package's change log file.

Several data sets from various areas of science are integrated in \pkg{QCA} so
as to facilitate users' familiarization with the package's functionality. Areas
currently covered are business, management & organization (\code{\link{d.CZH}}),
education (\code{\link{d.SS}}), environmental sciences (\code{\link{d.Bas}}),
evaluation (\code{\link{d.SA}}), legal studies (\code{\link{d.AS}}), political
science (\code{\link{d.Emm}}, \code{\link{d.HK}}, \code{\link{d.Kro}}),public
health (\code{\link{d.BWB}}), urban affairs (d.Kil), and sociology
(\code{\link{d.CS}}, \code{\link{d.HMN}}). For more details, see the data sets'
documentation files.

The official reference for the QCA package is the software citation provided
by the command \code{citation("QCA")}. How to make the most of the package's capabilities
is demonstrated in detail by Thiem and Dusa (2013c) with examples from political science
(note that data sets have been renamed), and users are also encouraged to check the
article published in the R Journal. Later versions of the package than those used in
Thiem and Dusa (2013b) and Thiem and Dusa (2013c) may feature slightly different functions,
arguments, values and default settings but backward compatibility is generally ensured.
}

\details{
    \tabular{ll}{
    Package: \tab QCA\cr
    Type:    \tab Package\cr
    Version: \tab 1.1-3.2\cr
    Date:    \tab 2014-06-23\cr
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
Baumgartner, Michael. 2009a. Uncovering Deterministic Causal Structures: A 
Boolean Approach. 
\emph{Synthese} 170 (1):71-96.

Baumgartner, Michael. 2009b. Inferring Causal Complexity. 
\emph{Sociological Methods & Research} 38 (1):71-101.

Caren, Neal, and Aaron Panofsky. 2005. TQCA: A Technique for Adding Temporality 
to Qualitative Comparative Analysis. 
\emph{Sociological Methods & Research} 34 (2):147-72.

Cronqvist, Lasse, and Dirk Berg-Schlosser. 2009. Multi-Value QCA (mvQCA). In 
\emph{Configurational Comparative Methods: Qualitative Comparative Analysis (QCA) 
and Related Techniques}, eds. B. Rihoux and C. C. Ragin. London: Sage Publications, 
pp. 69-86.

Dusa, Adrian. 2007. \emph{Enhancing Quine-McCluskey}. COMPASSS: WP 2007-49.
Available from: http://www.compasss.org/wpseries/Dusa2007b.pdf.

Dusa, Adrian. 2010. A Mathematical Approach to the Boolean Minimization Problem. 
\emph{Quality & Quantity} 44 (1):99-113.

Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Ragin, Charles C. 2000. \emph{Fuzzy-Set Social Science}. Chicago: University of Chicago Press.

Ragin, Charles C. 2008. \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
Chicago: University of Chicago Press.

Ragin, Charles C., and Sarah Ilene Strand. 2008. Using Qualitative Comparative 
Analysis to Study Causal Order: Comment on Caren and Panofsky (2005). 
\emph{Sociological Methods & Research} 36 (4):431-41.

Thiem, Alrik, and Adrian Dusa. 2013a. Boolean Minimization in Social Science Research:
A Review of Current Software for Qualitative Comparative Analysis (QCA). 
\emph{Social Science Computer Review} 31 (4):505-21.

Thiem, Alrik, and Adrian Dusa. 2013b. QCA: A Package for Qualitative Comparative
Analysis. 
\emph{The R Journal} 5 (1):87-97.

Thiem, Alrik, and Adrian Dusa. 2013c. \emph{Qualitative Comparative Analysis with R:
A User's Guide}. New York: Springer.
}

\keyword{package}

