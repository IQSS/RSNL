\name{FunctionalTokenFilter-class}
\docType{class}
\alias{FunctionalTokenFilter-class}
\alias{filterTokens,character,FunctionalTokenFilter,missing,missing-method}
\alias{func,FunctionalTokenFilter-method}
\alias{isAppliedToEach,FunctionalTokenFilter-method}
\alias{isNegated,FunctionalTokenFilter-method}

\title{Class "FunctionalTokenFilter"}

\description{
  A \code{\linkS4class{TokenFilter}} that allows the user to employ an
  arbitrary function as a token filter.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{FunctionalTokenFilter(fun, negate, applyToEach)}.
}

\section{Extends}{
Class \code{"\linkS4class{TokenFilter}"}, directly.
Class \code{"\linkS4class{TagSafe}"}, directly.
Class \code{"\linkS4class{TokenTransform}"}, by class "TokenFilter", distance 2.
}


\section{Methods}{
  \describe{
    \item{filterTokens}{\code{signature(object = "character",
                              filter = "FunctionalTokenFilter",
                              tokenizer = "missing",
                              index = "missing")}:
      Applies the filter to a \code{character} vector of tokens and 
      returns a \code{character} vector of filtered tokens.  Note that
      this method will apply the default tokenizer to generate a token
      vector if \code{object} happens to be a
      \code{\linkS4class{TextDocument}}.}

    \item{func}{\code{signature(object = "FunctionalTokenFilter")}:
      Returns the function used by the filter to indicate tokens to keep. }

    \item{isAppliedToEach}{\code{signature(object =
                                  "FunctionalTokenFilter")}:
      Sets the value passed to the function encapsulated by the
      filter.  When set to \code{TRUE} the function should take a
      \code{character} vector of length 1 and return a corresponding
      \code{logical} of length 1.  Otherwise, the function should
      expect a vector of multiple tokens and return a \code{logical}
      vector of the same length.}
      
    \item{isNegated}{\code{signature(object =
                                  "FunctionalTokenFilter")}:
      When this is \code{TRUE} the filter keeps those tokens the 
      filtering functions indicates it should not keep.}
  }
}

\details{
  A \code{FunctionalTokenFilter} object encapsulates a simple function
  and uses it to filter a sequence of tokens.  The function should
  take a single \code{character} vector (one token per element) as its
  argument and return a \code{logical} vector of the same length as
  its input, indicating which tokens to keep.  If \code{negate} is set
  to true, this returned vector will, instead, indicate which tokens
  to filter out.  If \code{isAppliedToEach} is set to true, the
  function should expect only a single token and return a single
  \code{logical}.  \code{FunctionalTokenFilter} objects are tag-safe;
  that is they behave appropriately when used to filter
  \code{\linkS4class{TaggedView}} or \code{\linkS4class{Tagged}}
  objects.
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{FunctionalTokenFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{FunctionalTagFilter}}
}


\keyword{classes}
