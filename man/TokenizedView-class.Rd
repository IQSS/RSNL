\name{TokenizedView-class}
\docType{class}
\alias{TokenizedView-class}
\alias{tokenFilters,TokenizedView-method}
\alias{tokenizer,TokenizedView-method}
\alias{tokenTransforms,TokenizedView-method}

\title{Class "TokenizedView"}

\description{
  An abstract class representing a view of another object as a
  sequence of tokens.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    
   \item{tokenFilters}{\code{signature(object = "TokenizedView")}:
    Returns a list of all of the \code{\linkS4class{TokenFilter}}
    objects applied to this view, in order.}

    \item{tokenizer}{\code{signature(object = "TokenizedView")}:
    Returns the \code{\linkS4class{Tokenizer}} used by this view.}

   \item{tokenTransforms}{\code{signature(object = "TokenizedView")}:
    Returns a list of all of the \code{\linkS4class{TokenTransform}}
    objects applied to this view, in order.  This includes
    \code{\linkS4class{TokenFilter}}s which are a sub-type of
    \code{\linkS4class{TokenTransform}}}
  }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tokenize}}
  \code{\linkS4class{Tokenizer}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
