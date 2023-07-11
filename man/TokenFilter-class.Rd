\name{TokenFilter-class}
\docType{class}
\alias{TokenFilter-class}
\alias{TokenFilter}
\alias{filterTokens,character,TokenFilter,ANY,ANY-method}
\alias{transformTokens,character,TokenFilter,missing,missing-method}

\title{Class "TokenFilter"}

\description{
  An abstract class representing an object that takes a sequence of
  tokens and returns a subsequence of the original.
}

\section{Extends}{
Class \code{"\linkS4class{TokenTransform}"}, directly.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{transformTokens}{\code{signature(object = "character",
                                        transform = "TokenFilter",
                                        tokenizer = "missing",
                                        index = "missing")}:
      Helps implement the \code{\linkS4class{TokenTransform}}
      interface.  See the note}

    
    \item{filterTokens}{\code{signature(object = "ANY",
                                        filter = "TokenFilter")}:
      Helps implement the \code{\linkS4class{TokenTransform}}
      interface.  See the note}
   }
}

\details{\code{TokenFilter} objects are a sub-type of
\code{\linkS4class{TokenTransform}} that specifically filter out
particular tokens from a set without performing any modifications to
the tokens themselves.
}

\note{
\code{TokenFilter} objects are a sub-type of
\code{\linkS4class{TokenTransform}} and thus must implement the
\code{\linkS4class{TokenTransform}} interface.  That is, they perform
work by providing a \code{signature(object = "character", transform =
"TokenFilter", tokenizer = "missing", index = "missing")}
specialization of \code{\link{transformTokens}}.  Nonetheless, users will
typically invoke \code{TokenFilters}s through
\code{\link{filterTokens}} and we'd like sub-classes of
\code{TokenFilter} to only need to provide a \code{character}
specialization of \code{\link{filterTokens}}.  The two method
specializations above make this possible by redirecting base case
calls to \code{\link{transformTokens}} to \code{\link{filterTokens}}
while redirecting all other calls to \code{\link{filterTokens}} to
\code{\link{transformTokens}}.  How this all works is largely
unimportant; the key thing to remember is that classes derived from
\code{TokenFilter} should implement \code{\link{filterTokens}}, not
\code{\link{transformTokens}}.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{filterTokens}}
  \code{\linkS4class{TokenTransform}}
  \code{\linkS4class{FunctionalTokenFilter}}
  \code{\linkS4class{RegexTokenFilter}}
  \code{\linkS4class{StopFilter}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}



\keyword{classes}
