\name{RegexTagFilter-class}
\docType{class}
\alias{RegexTagFilter-class}
\alias{pattern,RegexTagFilter-method}

\title{Class "RegexTagFilter"}
\description{
  A \code{\linkS4class{FunctionalTagFilter}} that retains tokens
 with associated tags that  match a regular expression.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{RegexTagFilter(pattern, negate)}.
}


\section{Extends}{
Class \code{"\linkS4class{FunctionalTagFilter}"}, directly.
Class \code{"\linkS4class{TokenFilter}"}, by class "FunctionalTagFilter", distance 2.
Class \code{"\linkS4class{TagSafe}"}, by class "FunctionalTagFilter", distance 2.
Class \code{"\linkS4class{TokenTransform}"}, by class "FunctionalTagFilter", distance 3.
}
\section{Methods}{
  \describe{
    \item{pattern}{\code{signature(object = "RegexTagFilter")}:
      Returns the \code{\linkS4class{Pattern}} used to drive this
      filter.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{RegexTagFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{FunctionalTagFilter}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{RegexTokenFilter}}
}

\keyword{classes}
