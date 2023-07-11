\name{TaggedView-class}
\docType{class}
\alias{TaggedView-class}
\alias{TaggedView}
\alias{tagger,TaggedView-method}
\alias{tokenizedView,TaggedView-method}

\title{Class "TaggedView"}

\description{
  An abstract class representing a view of another object as a
  sequence of labels, or tags.  \code{TaggedView} objects
  extend \code{\linkS4class{TokenizedView}} and provide access to both
  tokens and tags.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{tagger}{\code{signature(object = "TaggedView")}:
      Returns the \code{\linkS4class{Tagger}} used by this view. }
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tag}}
  \code{\linkS4class{Tagger}}
  \code{\linkS4class{TaggedDocumentView}}
  \code{\linkS4class{TaggedCorpusView}}
  \code{\linkS4class{Tagged}}
}

\keyword{classes}


