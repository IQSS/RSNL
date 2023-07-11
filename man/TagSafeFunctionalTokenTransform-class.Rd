\name{TagSafeFunctionalTokenTransform-class}
\docType{class}
\alias{TagSafeFunctionalTokenTransform-class}

\title{Class "TagSafeFunctionalTokenTransform"}
\description{A \code{\linkS4class{FunctionalTokenFilter}} that
guarantees proper handling \code{\linkS4class{Tagged}} token
representations.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{FunctionalTokenFilter(fun, TRUE, applyToEach)}.  See
\code{\link{FunctionalTokenFilter}} for details.
}


\section{Extends}{
Class \code{"\linkS4class{FunctionalTokenTransform}"}, directly.
Class \code{"\linkS4class{TagSafe}"}, directly.
Class \code{"\linkS4class{TokenTransform}"}, by class "FunctionalTokenTransform", distance 2.
}

\details{
  A \code{TagSafeFunctionalTokenTransform} object encapsulates a
  simple function and uses it to transform a sequence of tokens.  It
  is a specialization of the
  \code{\linkS4class{FunctionalTokenTransform}} class and behaves just
  like any other FTT except that it guarantees proper handling of
  \code{\linkS4class{Tagged}}.  The function encapsulated by a
  \code{TagSafeFunctionalTokenTransform} should take a single
  \code{character} (potentially of subtype
  \code{\linkS4class{Tagged}} vector (one token per element) as
  its argument and return a \code{character}
  (\code{\linkS4class{Tagged}}vector of post-transform tokens.
  If \code{isAppliedToEach} is set to true, the function should expect
  only a single token and return a length 1 vector.

}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{FunctionalTokenTransform}}
  \code{\linkS4class{FunctionalTokenTransform}}
  \code{\link{transformTokens}}
  \code{\linkS4class{TokenTransform}}
  \code{\linkS4class{RegexTokenTransform}}
  \code{\linkS4class{TolowerTokenTransform}}
  \code{\linkS4class{Tagged}}
}

\keyword{classes}
