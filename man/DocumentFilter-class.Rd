\name{DocumentFilter-class}
\docType{class}
\alias{DocumentFilter-class}
\alias{DocumentFilter}
\alias{filterDocuments,list,DocumentFilter-method}
\alias{filterDocuments,TokenizedCorpusView,DocumentFilter-method}
\alias{filterDocuments,RSNLCorpus,DocumentFilter-method}

\title{Class "DocumentFilter"}

\description{A virtual base class for filters that hide documents
matching some criterion from corpus views.}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{filterDocuments}{\code{signature(object = "list",
                                           filter = "DocumentFilter")}:
      Applies a \code{DocumentFilter} to a \code{list} of
      \code{\linkS4class{DocumentView}} objects.  Returns a filtered
      list of views.  All derived classes must override this method.}

    \item{filterDocuments}{\code{signature(object = "TokenizedCorpusView",
                                           filter = "DocumentFilter")}:
      Takes a \code{\linkS4class{TokenizedCorpusView}} and returns a
      similar view with some
      \code{\linkS4class{TokenizedDocumentView}}s filtered out.}

    \item{filterDocuments}{\code{signature(object = "RSNLCorpus",
                                           filter = "DocumentFilter")}:
      Generates a \code{\linkS4class{TokenizedCorpusView}}, using the
      given \code{DocumentFilter} to hide certain documents within the
      original corpus from view.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{filterDocuments}}
  \code{\linkS4class{FunctionalDocumentFilter}}
  \code{\linkS4class{RegexDocumentFilter}}
}

\keyword{classes}
