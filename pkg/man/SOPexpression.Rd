\name{Interpret SOP expressions: translate, compute, sop}

\alias{translate}
\alias{compute}
\alias{sop}

\title{Functions to interpret or create a SOP expression}

\description{
These functions interpret an expression written in a SOP (sum of products) form, for both
crisp and multivalue QCA. The function \bold{\code{translate()}} translates the expression
into a standard (canonical) SOP form using a matrix of implicants, while
\bold{\code{compute()}} uses the first to compute the scores based on a particular data input.

For crisp sets notation, upper case letters are considered the presence of that causal
condition, and lower case letters are considered the absence of the respective causal
condition. Tilde is recognized as a negation, even in combination with upper/lower letters.

Functions similar to \bold{\code{translate()}} and \bold{\code{compute()}} have initially
been written by Lewandowski (2015) but the actual code in these functions has been
completely re-written and expanded with more extensive functionality (see details and
examples below).

The function \bold{\code{sop()}} transforms any expression (most notably a POS - product
of sums) into a sum of products, minimizing it to the simplest equivalent logical expression.
It provides a software implementation of the intersection examples presented by
Ragin (1987: 144-147), and extended to multi-value sets.
}

\usage{
translate(expression = "", snames = "", noflevels, data)

compute(expression = "", data, separate = FALSE)

sop(expression = "", snames = "", use.tilde = FALSE, noflevels)
}

\arguments{
  \item{expression}{String: a QCA expression written in sum of products form.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{use.tilde}{Logical, use tilde to negate bivalent conditions.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{data}{A dataset with binary cs, mv and fs data.}
  \item{separate}{Logical, perform computations on individual, separate paths.}
}

\details{
A SOP - sum of products is also known as a DNF - disjunctive normal form, or in other
words a "union of intersections", for example \bold{\code{A*D + B*c}}.

The same expression can be written in multivalue notation: \bold{\code{A{1}*D{1} + B{1}*C{0}}}.
Both types of expressions are valid, and yield the same result on the same dataset.

For multivalue notation, causal conditions are expected as upper case letters, and they will be
converted to upper case by default. Expressions can contain multiple values to translate, separated
by a comma. If B was a multivalue causal condition, an expression could be:
\bold{\code{A{1} + B{1,2}*C{0}}}.

In this example, all values in B equal to either 1 or 2 will be converted to 1, and the
rest of the (multi)values will be converted to 0.

These functions automatically detects the use of tilde \dQuote{\code{~}} as a negation for a particular
causal condition. \bold{\code{~A}} does two things: it identifies the presence of causal
condition \bold{\code{A}} (because it was specified as upper case) and it recognizes that it
must be negated, because of the  tilde. It works even combined with lower case names:
\bold{\code{~a}}, which is interpreted as \bold{\code{A}}.

To negate a multivalue condition using a tilde, the number of levels should be supplied
(see examples below). Improvements in version 2.5 allow for intersections
between multiple levels of the same condition. For a causal condition with 3 levels (0, 1 and 2)
the following expression \bold{\code{~A{0,2}*A{1,2}}} is equivalent with \bold{\code{A{1}}}, while
\bold{\code{A{0}*A{1}}} results in the empty set.

The number of levels, as well as the set names can be automatically detected from a dataset via
the argument \bold{\code{data}}. Arguments \bold{\code{snames}} and \bold{\code{noflevels}} have
precedence over \bold{\code{data}}, when specified.

The use of the product operator \bold{\code{*}} is redundant when the set names are single
letters (for example \bold{\code{AD + Bc}}), and is also redundant for multivalue data, where 
product terms can be separated by using the curly brackets notation.

When conditions are binary and their names have multiple letters (for example
\bold{\code{AA + CC*bb}}), the use of the product operator \bold{\code{*}} is preferable but the
function manages to translate an expression even without it (\bold{\code{AA + CCbb}}) by searching
deep in the space of the conditions' names, at the cost of slowing down for a high number of causal
conditions. For this reason, an arbitrary limit of 7 causal \bold{\code{snames}} is imposed, to
write an expression.

For the function \bold{\code{sop()}}, if a tilde is present in the expression, the argument
\bold{\code{use.tilde}} is automatically activated.
}


\value{
For the function \bold{\code{translate()}}, a matrix containing the implicants on the
rows and the set names on the columns, with the following codes:
\tabular{rl}{
     0 \tab absence of a causal condition\cr
     1 \tab presence of a causal condition\cr
    -1 \tab causal condition was eliminated
}
The matrix was also assigned a class "translate", to avoid printing the -1 codes when signaling
a minimized condition. The mode of this matrix is character, to allow printing multiple levels
in the same cell, such as "1,2".
}

\author{
Adrian Dusa
}

\references{
Ragin, C.C. (1987) \emph{The Comparative Method: Moving beyond Qualitative and 
Quantitative Strategies}. Berkeley: University of California Press.

Lewandowski, J. (2015) QCAtools: Helper functions for QCA in R. R package version 0.1
}

\examples{

translate("A + B*C")

# same thing in multivalue notation
translate("A{1} + B{1}*C{1}")

# using upper/lower letters
translate("A + b*C")

# the negation with tilde is recognised
translate("~A + b*C")

# even in combination of upper/lower letters
translate("~A + ~b*C")

# and even for multivalue variables
# in multivalue notation, the product sign * is redundant
translate("C{1} + T{2} + T{1}V{0} + C{0}")

# negation of multivalue sets requires the number of levels
translate("~A{1} + ~B{0}*C{1}", snames = "A, B, C", noflevels = c(2,2,2))

# multiple values can be specified
translate("C{1} + T{1,2} + T{1}V{0} + C{0}")

# or even negated
translate("C{1} + ~T{1,2} + T{1}V{0} + C{0}", snames = "C, T, V", noflevels = c(2,3,2))

# if the expression does not contain the product sign *
# snames are required to complete the translation 
translate("AB + cD", snames = "A, B, C, D")

# otherwise snames are not required
translate("PER*FECT + str*ing")

# snames are required
translate("PERFECT + string", snames = "PER, FECT, STR, ING")

# it works even with overlapping columns
# SU overlaps with SUB in SUBER, but the result is still correct
translate("SUBER + subset", "SU, BER, SUB, SET")

\dontrun{
# error because combinations of condition names clash
translate("SUPER + subset", "SUP, ER, SU, PER, SUB, SET")
}


# to print _all_ codes from the standard output matrix
(obj <- translate("A + b*C"))
print(obj, original = TRUE) # also prints the -1 code



# -----
# for compute()
data(LF)
compute("DEV*ind + URB*STB", data = LF)

# calculating individual paths
compute("DEV*ind + URB*STB", data = LF, separate = TRUE)



# -----
# for sop()
sop("(A + B)(A + ~B)") # result is "A"

# to force a certain order of the set names
sop("(URB + LIT*~DEV)(~LIT + ~DEV)", snames = "DEV, URB, LIT")

# multilevel conditions can also be specified (and negated)
sop("(A{1} + ~B{0})(B{1} + C{0})", snames = "A, B, C", noflevels = c(2, 3, 2))


# in Ragin's (1987) book, the equation E = SG + LW is the result
# of the Boolean minimization for the ethnic political mobilization.

# intersecting the reactive ethnicity perspective (R = lw)
# with the equation E (page 144)

sop("lw(SG + LW)", snames = "S, L, W, G")

# [1] "SlwG"


# resources for size and wealth (C = SW) with E (page 145)
sop("SW(SG + LW)", snames = "S, L, W, G")

# [1] "SWG + SLW"


# and factorized
factorize(sop("SW(SG + LW)", snames = "S, L, W, G"))

# F1: SW(G + L)


# developmental perspective (D = Lg) and E (page 146)
sop("Lg(SG + LW)", snames = "S, L, W, G", use.tilde = TRUE)

# [1] "LW~G"
}

\keyword{functions}
