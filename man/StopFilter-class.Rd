\name{StopFilter-class}
\docType{class}
\alias{StopFilter-class}
\alias{stoplist,StopFilter-method}

\title{Class "StopFilter"}

\description{
  A \code{\linkS4class{FunctionalTokenFilter}} that filters a list of
  tokens by removing any tokens that are in a list of stop-words.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{StopFilter(language)}.
}

\section{Extends}{
Class \code{"\linkS4class{FunctionalTokenFilter}"}, directly.
Class \code{"\linkS4class{TokenFilter}"}, by class "FunctionalTokenFilter", distance 2.
Class \code{"\linkS4class{TokenTransform}"}, by class
"FunctionalTokenFilter", distance 3.
}

\section{Methods}{
  \describe{
    \item{stoplist}{\code{signature(object = "StopFilter")}:
      Returns a \code{character} vector of stop-words.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{StopFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{FunctionalTokenFilter}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
