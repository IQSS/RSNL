\name{RegexDocumentFilter-class}
\docType{class}
\alias{RegexDocumentFilter-class}
\alias{pattern,RegexDocumentFilter-method}

\title{Class "RegexDocumentFilter"}

\description{
  A \code{\linkS4class{FunctionalDocumentFilter}} that retains
  document views that match a regular expression.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{RegexDocumentFilter(pattern, negate)}.
}

\section{Extends}{
Class \code{"\linkS4class{FunctionalDocumentFilter}"}, directly.
Class \code{"\linkS4class{DocumentFilter}"}, by class "FunctionalDocumentFilter", distance 2.
}

\section{Methods}{
  \describe{
    \item{pattern}{\code{signature(object = "RegexDocumentFilter")}:
      Returns the \code{\linkS4class{Pattern}} used to drive this
      filter.}
  }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{RegexDocumentFilter}}
  \code{\link{filterDocuments}}
  \code{\linkS4class{FunctionalDocumentFilter}}
  \code{\linkS4class{DocumentFilter}}
}

\keyword{classes}
