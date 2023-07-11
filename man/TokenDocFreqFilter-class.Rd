\name{TokenDocFreqFilter-class}
\docType{class}
\alias{TokenDocFreqFilter-class}
\alias{maxFreq,TokenDocFreqFilter-method}
\alias{minFreq,TokenDocFreqFilter-method}

\title{Class "TokenDocFreqFilter"}

\description{
  A \code{\linkS4class{FunctionalTokenFilter}} that filters a list of
  tokens by removing tokens that appear outside a prescribed frequency
  within a particular \code{\linkS4class{TokenizedCorpusView}}.
}

\section{Objects from the Class}{
Objects can be create by calls of the form
\code{TokenDocFreqFilter(view, min, max)}.
}

\section{Extends}{
Class \code{"\linkS4class{FunctionalTokenFilter}"}, directly.
Class \code{"\linkS4class{TokenFilter}"}, by class "FunctionalTokenFilter", distance 2.
Class \code{"\linkS4class{TokenTransform}"}, by class "FunctionalTokenFilter", distance 3.
}

\section{Methods}{
  \describe{
    \item{maxFreq}{\code{signature(object = "TokenDocFreqFilter")}:
      Returns the maximum frequency of tokens to keep.}
          
    \item{minFreq}{\code{signature(object = "TokenDocFreqFilter")}:
      Returns the minimum frequency fo tokens to keep.}
  }
}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{TokenDocFreqFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{FunctionalTokenFilter}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
