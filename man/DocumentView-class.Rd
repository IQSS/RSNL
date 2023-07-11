\name{DocumentView-class}
\docType{class}
\alias{DocumentView-class}
\alias{corpus,DocumentView-method}
\alias{document,DocumentView-method}
\alias{index,DocumentView-method}

\title{Class "DocumentView"}
\description{
  An abstract class representing a view of a
  \code{\linkS4class{TextDocument}}.  \code{DocumentView} classes are
  used to view the content of a \code{\linkS4class{TextDocument}} in a
  new way.  \code{DocumentView} objects are linked to a specific corpus,
  and may only view documents contained within
  \code{\linkS4class{RSNLCorpus}} objects.
}

\section{Extends}{
Class \code{"\linkS4class{View}"}, directly.
}


\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{document}{\code{signature(object = "DocumentView")}: Returns
    the \code{\linkS4class{TextDocument}} object viewed by the
    \code{DocumentView}.}
    \item{index}{\code{signature(object = "DocumentView")}: Returns
    the location, within the corpus, of the viewed document.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{View}}
  \code{\linkS4class{TextDocument}}
  \code{\linkS4class{RSNLCorpus}}
  \code{\linkS4class{CorpusView}}
}

\keyword{classes}
