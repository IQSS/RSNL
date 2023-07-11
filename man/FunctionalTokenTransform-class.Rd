\name{FunctionalTokenTransform-class}
\docType{class}
\alias{FunctionalTokenTransform-class}
\alias{func,FunctionalTokenTransform-method}
\alias{isAppliedToEach,FunctionalTokenTransform-method}
\alias{show,FunctionalTokenTransform-method}
\alias{transformTokens,character,FunctionalTokenTransform,missing,missing-method}

\title{Class "FunctionalTokenTransform"}

\description{
  A \code{\linkS4class{TokenTransform}} that allows the user to employ an
  arbitrary function as a token filter.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{FunctionalTokenTransform(fun, tagSafe, applyToEach)}.  See
  \code{\link{FunctionalTokenFilter}} for details.
}

\section{Extends}{
Class \code{"\linkS4class{TokenTransform}"}, directly.
}

\section{Methods}{
  \describe{
    \item{func}{\code{signature(object = "FunctionalTokenTransform")}:
      Returns the function used by the transform.}

    \item{isAppliedToEach}{\code{signature(object =
                              "FunctionalTokenTransform")}:
      Sets the value passed to the function encapsulated by the
      transform  When set to \code{TRUE} the function should take a
      \code{character} vector of length 1 and return a corresponding
      \code{character} of length 1.  Otherwise, the function should
      expect a vector of multiple tokens and return a \code{character}
      vector, possibly of different length.}

    \item{transformTokens}{\code{signature(object = "character",
                                transform = "FunctionalTokenTransform",
                                tokenizer = "missing", index = "missing")}:
      Applies the transform to a \code{character} vector of tokens and
      returns a \code{character} vector of transformed tokens.  Note
      that this method will apply the default tokenizer to generate a
      token vector if \code{object} happens to be a
      \code{\linkS4class{TextDocument}}.}

	 }
}

\details{
  A \code{FunctionalTokenTransform} object encapsulates a simple function
  and uses it to transform a sequence of tokens.  The function should
  take a single \code{character} vector (one token per element) as its
  argument and return a \code{character} vector of post-transform tokens. 
  If \code{isAppliedToEach} is set to true, the function should expect
  only a single token and return a length 1 \code{character} vector.
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{FunctionalTokenTransform}}
  \code{\linkS4class{TagSafeFunctionalTokenTransform}}
  \code{\link{transformTokens}}
  \code{\linkS4class{TokenTransform}}
  \code{\linkS4class{RegexTokenTransform}}
  \code{\linkS4class{TolowerTokenTransform}}
}

\keyword{classes}
