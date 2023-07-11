\name{FunctionalTagFilter-class}
\docType{class}
\alias{FunctionalTagFilter-class}
\alias{filterTokens,character,FunctionalTagFilter,missing,missing-method}
\alias{filterTokens,Tagged,FunctionalTagFilter,missing,missing-method}
\alias{func,FunctionalTagFilter-method}
\alias{isAppliedToEach,FunctionalTagFilter-method}
\alias{isNegated,FunctionalTagFilter-method}
\alias{show,FunctionalTagFilter-method}

\title{Class "FunctionalTagFilter"}

\description{
  A \code{\linkS4class{TokenFilter}} that allows the user to employ an
  arbitrary function to filter a series of tokens based on tags.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{FunctionalTagFilter(fun, negate, applyToEach)}.
}

\section{Extends}{
Class \code{"\linkS4class{TokenFilter}"}, directly.
Class \code{"\linkS4class{TagSafe}"}, directly.
Class \code{"\linkS4class{TokenTransform}"}, by class "TokenFilter", distance 2.
}

\section{Methods}{
  \describe{
    \item{filterTokens}{\code{signature(object = "Tagged", filter = "FunctionalTagFilter", tokenizer = "missing", index = "missing")}:
      Applies the filter to a \code{\linkS4class{Tagged}} vector of
      tokens and returns a \code{\linkS4class{Tagged}} vector of
      filtered (tagged) tokens.  This method filters tokens based on
      their tags, not on their actual values.  Note that one cannot
      use \code{filterTokens} to apply a \code{FunctionalTagFilter} to
      objects that do not extend the \code{\linkS4class{Tagged}} or
      \code{\linkS4class{TaggedView}} types.}
    
    \item{func}{\code{signature(object = "FunctionalTagFilter")}:
      Returns the function used by the filter to indicate tokens to keep. }

    \item{isAppliedToEach}{\code{signature(object =
                                  "FunctionalTagFilter")}:
      Sets the value passed to the function encapsulated by the
      filter.  When set to \code{TRUE} the function should take a
      \code{\linkS4class{Tagged}} vector of length 1 and return a
      corresponding \code{logical} of length 1.  Otherwise, the
      function should expect a vector of multiple tagged tokens and
      return a \code{logical} vector of the same length.}
      
    \item{isNegated}{\code{signature(object =
                                  "FunctionalTagFilter")}:
      When this is \code{TRUE} the filter keeps those tagged tokens the 
      filtering functions indicates it should not keep.}
  }
}

\details{ A \code{FunctionalTagFilter} object encapsulates a simple
function and uses it to filter a sequence of tokens, based on the
values of associated tags.  The function should take a single
\code{\linkS4class{Tagged}} vector (one tagged token per element) as
its argument and return a \code{logical} vector of the same length as
its input, indicating which tokens to keep.  If \code{negate} is set
to true, this returned vector will, instead, indicate which tokens to
filter out.  If \code{isAppliedToEach} is set to true, the function
should expect only a single token and return a single \code{logical}.
\code{FunctionalTokenFilter} objects only work with tagged types and
cannot be used to filter objects that do not extend the
\code{\linkS4class{TaggedView}} or \code{\linkS4class{Tagged}}
classes.  }

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{FunctionalTagFilter}}
  \code{\link{filterTokens}}
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{FunctionalTokenFilter}}
}


\keyword{classes}
