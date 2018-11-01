\name{QCA-internal}

\alias{.onAttach}

\alias{agteb}
\alias{alteb}
\alias{as.panel}
\alias{combinations}
\alias{minimizeLoop}
\alias{negateLoop}
\alias{prettyTable}
\alias{print.aE}
\alias{print.deMorgan}
\alias{print.factorize}
\alias{print.fuzzy}
\alias{print.pic}
\alias{print.pof}
\alias{print.qca}
\alias{print.mqca}
\alias{print.sS}
\alias{print.tt}
\alias{rebuild}
\alias{removeRedundants}
\alias{setRownames}
\alias{setColnames}
\alias{setDimnames}
\alias{uninstall}
\alias{validateNames}
\alias{verify.data}
\alias{verify.dir.exp}
\alias{verify.minimize}
\alias{verify.inf.test}
\alias{verify.qca}
\alias{verify.mqca}
\alias{verify.multivalue}
\alias{verify.tt}

\alias{d.AS}
\alias{d.Bas}
\alias{d.biodiversity}
\alias{d.BWB}
\alias{d.CS}
\alias{d.CZH}
\alias{d.education}
\alias{d.Emm}
\alias{d.graduate}
\alias{d.health}
\alias{d.HK}
\alias{d.HMN}
\alias{d.homeless}
\alias{d.jobsecurity}
\alias{d.Kil}
\alias{d.Kro}
\alias{d.napoleon}
\alias{d.partybans}
\alias{d.represent}
\alias{d.RS}
\alias{d.SA}
\alias{d.socialsecurity}
\alias{d.SS}
\alias{d.stakeholder}
\alias{d.transport}
\alias{d.urban}
\alias{Emme}
\alias{HarKem}
\alias{Krook}
\alias{RagStr}
\alias{Rokkan}


\alias{asNumeric}
\alias{possibleNumeric}

\alias{dashes}
\alias{hastilde}
\alias{tilde1st}
\alias{notilde}
\alias{trimstr}
\alias{nec}
\alias{suf}
\alias{splitstr}
\alias{getInfo}
\alias{getLevels}
\alias{getName}
\alias{getBigList}
\alias{splitMainComponents}
\alias{splitBrackets}
\alias{removeSingleStars}
\alias{splitPluses}
\alias{splitStars}
\alias{splitTildas}
\alias{solveBrackets}
\alias{simplifyList}
\alias{negateValues}
\alias{removeDuplicates}
\alias{factor.function}
\alias{getNonChars}
\alias{splitProducts}
\alias{insideBrackets}
\alias{outsideBrackets}
\alias{curlyBrackets}
\alias{roundBrackets}


\alias{getSolution}
\alias{writeSolution}
\alias{prettyString}
\alias{rowDominance}
\alias{sortExpressions}
\alias{sortVector}
\alias{sortMatrix}
\alias{writePrimeimp}


\alias{GUIcall}


\title{Internal Functions}

\description{
  The above functions are internal in the QCA package which are not designed to
  be called directly by the user. All of them are used by the \code{minimize} function,
  except \code{sortMatrix} which is used by \code{allExpressions}. The verification
  and error messages have been moved to the internal functions \code{verify.data}
  and \code{verify.tt}.
}

\keyword{internal}
