\name{PunctTokenFilter-class}
\docType{class}
\alias{PunctTokenFilter-class}
\alias{show,PunctTokenFilter-method}

\title{Class "PunctTokenFilter"}

\description{
  A specialization of \code{\linkS4class{RegexTokenFilter}} that
  filters out all pure-punctuation tokens.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{PunctTokenFilter()}.
}

\section{Extends}{
Class \code{"\linkS4class{RegexTokenFilter}"}, directly.
Class \code{"\linkS4class{FunctionalTokenFilter}"}, by class "RegexTokenFilter", distance 2.
Class \code{"\linkS4class{TokenFilter}"}, by class "RegexTokenFilter", distance 3.
Class \code{"\linkS4class{TokenTransform}"}, by class "RegexTokenFilter", distance 4.
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{RegexTokenFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{FunctionalTokenFilter}} 
  \code{\linkS4class{TokenFilter}}
}


\keyword{classes}
