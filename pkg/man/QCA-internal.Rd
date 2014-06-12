\name{QCA-internal}

\alias{.onAttach}
\alias{base3rows}
\alias{createChart}
\alias{createString}
\alias{deMorganLoop}
\alias{eqmccLoop}
\alias{fuzzyand}
\alias{fuzzyor}
\alias{getSolution}
\alias{prettyString}
\alias{prettyTable}
\alias{print.aE}
\alias{print.deMorgan}
\alias{print.fctr}
\alias{print.pic}
\alias{print.pof}
\alias{print.qca}
\alias{print.mqca}
\alias{print.ss}
\alias{print.tt}
\alias{rowDominance}
\alias{sortVector}
\alias{sortMatrix}
\alias{verify.data}
\alias{verify.dir.exp}
\alias{verify.expl}
\alias{verify.inf.test}
\alias{verify.qca}
\alias{verify.mqca}
\alias{verify.tt}
\alias{writePrimeimp}
\alias{writeSolution}


\title{Internal Functions}

\description{
  The above functions are internal in the QCA package which are not designed to 
  be called directly by the user. All of them are used by the \code{eqmcc} function, 
  except \code{sortMatrix} which is used by \code{allExpressions}. The verification 
  and error messages have been moved to the internal functions \code{verify.data} 
  and \code{verify.tt}.
}

\keyword{internal}