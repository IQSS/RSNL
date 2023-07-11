\name{Tagged-class}
\docType{class}
\alias{Tagged-class}
\alias{flatten,Tagged-method}
\alias{label<-,Tagged-method}
\alias{label,Tagged-method}

\title{Class "Tagged"}

\description{An extensions of \code{character} that associates each
token with a string-valued tag.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{Tagged(value=character(), tag=character())} where \code{value}
and \code{tag} must be equal length vectors.
}

\section{Extends}{
Class \code{"\linkS4class{character}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "character", distance 2.
Class \code{"\linkS4class{data.frameRowLabels}"}, by class "character", distance 2.
}

\section{Methods}{
  \describe{
    \item{flatten}{\code{signature(object = "Tagged")}:
      Convert each value in \code{object} to a flat \code{character}
      representation of the form "token/tag".  Note that
      \code{as.character} returns a simple \code{character} vector of
      the tokens in \code{object}.}
    
    \item{label<-}{\code{signature(object = "Tagged")}:
      Set the tag(s) of \code{object}.  Input must be the same length
      as \code{object}.}
    
    \item{label}{\code{signature(object = "Tagged")}:
      Extract the tag(s) associated with the values in \code{object}.}
  }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{TaggedView}}
  \code{\linkS4class{TaggedDocumentView}}
  \code{\linkS4class{TaggedCorpusView}}
  \code{\linkS4class{Tagger}}
  \code{\linkS4class{TagSafe}}
}

\keyword{classes}
