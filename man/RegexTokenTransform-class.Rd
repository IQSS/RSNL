\name{RegexTokenTransform-class}
\docType{class}
\alias{RegexTokenTransform-class}
\alias{pattern,RegexTokenTransform-method}
\alias{replacementCount,RegexTokenTransform-method}
\alias{replacementString,RegexTokenTransform-method}

\title{Class "RegexTokenTransform"}

\description{
  A \code{\linkS4class{FunctionalTokenTransform}} that replaces
  regular expression matches in tokens with a given substitute string.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{RegexTokenTransform(pattern, repl="")}
}

\section{Extends}{
Class \code{"\linkS4class{TagSafeFunctionalTokenTransform}"}, directly.
Class \code{"\linkS4class{FunctionalTokenTransform}"}, by class "TagSafeFunctionalTokenTransform", distance 2.
Class \code{"\linkS4class{TagSafe}"}, by class "TagSafeFunctionalTokenTransform", distance 2.
Class \code{"\linkS4class{TokenTransform}"}, by class "TagSafeFunctionalTokenTransform", distance 3.
}

\section{Methods}{
  \describe{
    \item{pattern}{\code{signature(object = "RegexTokenTransform")}:
      Returns regular expression object driving the transform.}

    \item{replacementCount}{\code{signature(object = 
        "RegexTokenTransform")}:
      Returns the maximum number of pattern matches the transform will
      replace in a token.}

    \item{replacementString}{\code{signature(object =
        "RegexTokenTransform")}:
      Returns the string used by the transform to replace matches.
      This string may contain backreferences.}
  }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{transformTokens}}
  \code{\linkS4class{FunctionalTokenTransform}}
  \code{\linkS4class{TokenTransform}}
  \code{\link{RegexTokenFilter}}
}
\keyword{classes}
