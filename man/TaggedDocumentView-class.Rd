\name{TaggedDocumentView-class}
\docType{class}
\alias{TaggedDocumentView-class}
\alias{TaggedDocumentView}
\alias{[,TaggedDocumentView,ANY,ANY,ANY-method}
\alias{freq,TaggedDocumentView,character-method}
\alias{freq,TaggedDocumentView,Tagged-method}
\alias{freqTable,TaggedDocumentView-method}
\alias{has,TaggedDocumentView,character-method}
\alias{has,TaggedDocumentView,Tagged-method}
\alias{taggedTokens,TaggedDocumentView-method}
\alias{tags,TaggedDocumentView-method}
\alias{unique,TaggedDocumentView-method}

\title{Class "TaggedDocumentView"}

\description{
  A view of a \code{\linkS4class{TextDocument}} object (contained
  within a \code{\linkS4class{RSNLCorpus}}) as a sequence of token
  labels, or tags.
}

\section{Objects from the Class}{
    In general, \code{TaggedDocumentView} objects should not be
    constructed directly unless you are writing code to extend this
    package. Instead, they can be obtained from an existing 
    \code{\linkS4class{RSNLCorpus}} object by calling
    \code{tag(corpus, index=i)}.
}

\section{Extends}{
Class \code{"\linkS4class{TaggedView}"}, directly.
Class \code{"\linkS4class{TokenizedDocumentView}"}, directly.
Class \code{"\linkS4class{DocumentView}"}, by class
"TokenizedDocumentView", distance 2.
Class \code{"\linkS4class{TokenizedView}"}, by class
"TokenizedDocumentView", distance 2.
Class \code{"\linkS4class{View}"}, by class "TokenizedDocumentView",
distance 3.
}

\section{Methods}{
  \describe{
    \item{freq}{\code{signature(object = "TaggedDocumentView",
                                value = "character")}:
      By default, returns the frequency of the token-tag pair
      \code{value} within the \code{TaggedDocumentView}.  The pair
      should be written in the form "token/tag".  The method can also
      return the frequency of particular tokens or tags within the
      view.  The optional argument \code{what = c("TaggedTokens",
      "Tokens", "Tags")} indicates where the method should search for
      \code{value}.}
    
    \item{freq}{\code{signature(object = "TaggedDocumentView",
                                value = "Tagged")}:
      A specialization of \code{freq} for \code{\linkS4class{Tagged}}
      objects.  Returns the frequency of the tagged token, token, or
      tag represented by \code{value}, depending on the value of the
      optional argument \code{what = c("TaggedTokens", "Tokens",
      "Tags")}.}

    \item{freqTable}{\code{signature(object = "TaggedDocumentView")}:
      By default, returns a table of the frequenices of token-tag
      pairs within the \code{TaggedDocumentView}.  The method can also
      return a frequency table of tokens or tags.  The optional
      \code{what = c("TaggedTokens", "Tokens", "Tags")} indicates
      what type of frequency table the method should return.}

    \item{has}{\code{signature(object = "TaggedDocumentView",
                               value = "character")}:
      Returns \code{TRUE} if the token-tag pair \code{value} is
      contained within the \code{TaggedDocumentView}. The method can
      also indicate whether \code{object} contains particular tokens
      or tags.  The optional \code{what = c("TaggedTokens", "Tokens",
      "Tags")} indicates where the method should search for
      \code{value}.}

    \item{has}{\code{signature(object = "TaggedDocumentView",
                               value = "Tagged")}:
      A specialization of \code{has} for \code{\linkS4class{Tagged}}
      objects.  Returns \code{TRUE} if the tagged token, token, or
      tag represented by \code{value} is contained within the
      \code{TaggedDocumentView}, depending on the value of the
      optional argument \code{what = c("TaggedTokens", "Tokens",
      "Tags")}.}

    \item{taggedTokens}{\code{signature(object = "TaggedDocumentView")}:
      Returns a \code{character} vector containing the
      tagged tokens represented by the view, in "tag/token" format.}

    \item{tags}{\code{signature(object = "TaggedDocumentView")}:
      Returns a character vector of the \code{TaggedDocumentView}'s
      tags. }

    \item{unique}{\code{signature(x = "TaggedDocumentView")}:
      Returns a vector of the \code{TaggedDocumentView}'s
      unique token-tag pairs by default.  The method can also return
      unique tokens and tags.  The optional \code{what =
      c("TaggedTokens", "Tokens", "Tags")} indicates what set of
      unique values to return.}
	 }
}

\note{The \code{tokens} method inherited from
\code{\linkS4class{TokenizedDocumentView}} returns a vector of type
\code{\linkS4class{Tagged}} when invoked on \code{TaggedDocumentView}
objects.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tag}}
  \code{\linkS4class{DocumentView}}
  \code{\linkS4class{TaggedView}}
  \code{\linkS4class{TaggedCorpusView}}
  \code{\linkS4class{Tagged}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedCorpusView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{RSNLCorpus}}
}

\keyword{classes}
