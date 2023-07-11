\name{RegexTokenFilter-class}
\docType{class}
\alias{RegexTokenFilter-class}
\alias{pattern,RegexTokenFilter-method}

\title{Class "RegexTokenFilter"}

\description{
  A \code{\linkS4class{FunctionalTokenFilter}} that retains tokens
  that match a regular expression.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{RegexTokenFilter(pattern, negate)}.
}

\section{Extends}{
Class \code{"\linkS4class{FunctionalTokenFilter}"}, directly.
Class \code{"\linkS4class{TokenFilter}"}, by class "FunctionalTokenFilter", distance 2.
Class \code{"\linkS4class{TagSafe}"}, by class "FunctionalTokenFilter", distance 2. 
Class \code{"\linkS4class{TokenTransform}"}, by class "FunctionalTokenFilter", distance 3.

}

\section{Methods}{
  \describe{
    \item{pattern}{\code{signature(object = "RegexTokenFilter")}:
      Returns the \code{\linkS4class{Pattern}} used to drive this
      filter.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{RegexTokenFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{FunctionalTokenFilter}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{RegexTagFilter}}
}

\keyword{classes}
