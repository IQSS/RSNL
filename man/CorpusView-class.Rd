\name{CorpusView-class}
\docType{class}
\alias{CorpusView-class}
\alias{CorpusView}
\alias{[,CorpusView,ANY,missing,missing-method}
\alias{[[,CorpusView,ANY,missing-method}
\alias{documentViews,CorpusView-method}
\alias{length,CorpusView-method}
\alias{keepDocumentViews<-,CorpusView,logical-method}
\alias{keepDocumentViews,CorpusView-method}

\title{Class "CorpusView"}

\description{
  An abstract class representing a view of a
\code{\linkS4class{RSNLCorpus}}.
  \code{CorpusView} classes are used to view the content of a
  \code{\linkS4class{RSNLCorpus}} in a new way.  \code{CorpusView}
  objects maintain soft references to the corpora that they view and
  automatically update themselves to reflect changes in a viewed
  corpus.
}

\section{Extends}{
Class \code{"\linkS4class{View}"}, directly.
}

\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "CorpusView", i = "ANY", j =
    "missing", drop = "missing")}: Returns a slice of
    \code{\linkS4class{DocumentView}} objects.}

    \item{[[}{\code{signature(x = "CorpusView", i = "ANY", j = "missing")}:
    Returns a \code{\linkS4class{DocumentView}} object.}

    \item{documentFilters}{\code{signature(object = "CorpusView")}:
    Returns a list of all of the \code{\linkS4class{DocumentFilter}}
    objects applied to the \code{CorpusView}. }

    \item{documentViews}{\code{signature(object = "CorpusView")}:
    Returns a list containing all of the
    \code{\linkS4class{DocumentView}} objects held within the
    \code{CorpusView}. }
  
    \item{keepDocumentViews<-}{\code{signature(object = "CorpusView", value
    = "logical")}: When set to FALSE, directs the \code{CorpusView} to
    drop internally held \code{\linkS4class{DocumentView}} objects
    directly after computation.  When set to true, directs the object
    to store sub-views for future use. }

    \item{keepDocumentViews}{\code{signature(object = "CorpusView")}:
    Indicates whether this object will store computed
    \code{\linkS4class{DocumentView}} objects between calls.}

    \item{length}{\code{signature(x = "CorpusView")}: Returns the
    number of documents viewed by the \code{CorpusView}. }
	 }
}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{View}}
  \code{\linkS4class{TextDocument}}
  \code{\linkS4class{RSNLCorpus}}
  \code{\linkS4class{DocumentView}}
}

\examples{
  # Demonstrate auto-updating
  data(crude)
  pcrude <- RSNLCorpus(crude)
  tcrude <- tokenize(pcrude)
  toks1 <- unique(tcrude)
  tmMap(pcrude, tmTolower)
  isdiff <- length(toks1) == length(unique(tcrude))
}

\keyword{classes}
