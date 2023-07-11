\name{View-class}
\docType{class}
\alias{View-class}
\alias{View}
\alias{corpus,View-method}
\alias{keepComputed<-,View,logical-method}
\alias{keepComputed,View-method}

\title{Class "View"}

\description{
  An abstract class representing a view of some part or all of a
  \code{\linkS4class{RSNLCorpus}}.  This class is the parent of all
  \code{\linkS4class{CorpusView}} and
  \code{\linkS4class{DocumentView}} sub-types.
 }

\section{Methods}{
  \describe{
    \item{corpus}{\code{signature(object = "View")}: Returns the
    corpus referenced by this view.}

    \item{keepComputed<-}{\code{signature(object = "View", value =
    "logical")}:  When set to FALSE, directs the \code{View} to drop
    all computed data slots directly after us (e.g. the list of unique
    tokens computed by a \code{\linkS4class{TokenizedCorpusView}}.
    When set to TRUE, directs the object to store such computed values
    between calls.}

    \item{keepComputed}{\code{signature(object = "View")}: Indicates
    whether or not the object stores computed values between calls.}

	 }
}
\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}


\seealso{
  \code{\linkS4class{CorpusView}}
  \code{\linkS4class{DocumentView}}
}

\keyword{classes}
