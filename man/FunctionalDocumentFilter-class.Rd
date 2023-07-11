\name{FunctionalDocumentFilter-class}
\docType{class}
\alias{FunctionalDocumentFilter-class}
\alias{filterDocuments,list,FunctionalDocumentFilter-method}
\alias{func,FunctionalDocumentFilter-method}
\alias{isAppliedToEach,FunctionalDocumentFilter-method}
\alias{isNegated,FunctionalDocumentFilter-method}

\title{Class "FunctionalDocumentFilter"}

\description{
  A \code{\linkS4class{DocumentFilter}} that allows the user to employ
  an arbitrary function as a document filter.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{FunctionalDocumentFilter(fun, negate="FALSE", applyToEach="FALSE")}
}

\section{Extends}{
Class \code{"\linkS4class{DocumentFilter}"}, directly.
}
\section{Methods}{
  \describe{
    \item{filterDocuments}{\code{signature(object = "list",
                                      filter = "FunctionalDocumentFilter")}:
      Applies the filter to a \code{list} vector of
      \code{\linkS4class{DocumentView}} objects and 
      returns a filtered \code{list} of document views.}

    \item{func}{\code{signature(object = "FunctionalDocumentFilter")}:
      Returns the function used by the filter to indicate documents to keep. }

    \item{isAppliedToEach}{\code{signature(object =
                                  "FunctionalDocumentFilter")}:
      Sets the value passed to the function encapsulated by the
      filter.  When set to \code{TRUE} the function should take a
      single \code{\linkS4class{DocumentView}} object
      and return a corresponding
      \code{logical} of length 1, indicating whether or not to keep
      the document view in the filtered list.  Otherwise, the function
      should expect a \code{list} of multiple document views and return a
      \code{logical} vector of the same length as the \code{list}.}
      
    \item{isNegated}{\code{signature(object =
                                  "FunctionalDocumentFilter")}:
      When this is \code{TRUE} the filter keeps those documents the 
      filtering functions indicates it should not keep.}

	 }
}

\details{
  A \code{FunctionalDocumentFilter} object encapsulates a simple function
  and uses it to filter a collection of document views.  The function should
  take a list of \code{\linkS4class{DocumentView}}s as its only
  argument and return a \code{logical} vector of the same length as
  its input, indicating which views to keep.  If \code{negate} is set
  to true, this returned vector will, instead, indicate which document
  views to filter out.  If \code{isAppliedToEach} is set to true, the
  function should expect only a single
  \code{\linkS4class{DocumentView}} and return a single
  \code{logical} value.
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{FunctionalDocumentFilter}}
  \code{\link{filterDocuments}}
  \code{\linkS4class{DocumentFilter}}
}

\keyword{classes}
