\name{TokenizedDocumentView-class}
\docType{class}
\alias{TokenizedDocumentView-class}
\alias{TokenizedDocumentView}
\alias{[,TokenizedDocumentView,ANY,missing,ANY-method}
\alias{freq,TokenizedDocumentView,character-method}
\alias{freqTable,TokenizedDocumentView-method}
\alias{has,TokenizedDocumentView,character-method}
\alias{tokens,TokenizedDocumentView-method}
\alias{unique,TokenizedDocumentView-method}

\title{Class "TokenizedDocumentView"}

\description{
  View of a \code{\linkS4class{TextDocument}} object (contained within
  a \code{\linkS4class{RSNLCorpus}}) as a sequence of tokens.
}
\section{Objects from the Class}{
    In general, \code{TokenizedDocumentView} objects should not be
    constructed directly unless you are writing code to extend this
    package. Instead, they can be obtained from an existing 
    \code{\linkS4class{RSNLCorpus}} object by calling
    \code{tokenize(corpus, index=i)}.
}


\section{Extends}{
Class \code{"\linkS4class{DocumentView}"}, directly.
Class \code{"\linkS4class{TokenizedView}"}, directly.
Class \code{"\linkS4class{View}"}, by class "DocumentView", distance 2.
}

\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "TokenizedDocumentView", i = "ANY", j
                             = "missing", drop = "ANY")}:
      Returns specific tokens, or slices of tokens, at the given index
      \code{i}.}

    \item{freq}{\code{signature(object = "TokenizedDocumentView",
                                value = "character")}:
      Returns the frequency of the token \code{value} within the
      \code{TokenizedDocumentView}.}

    \item{freqTable}{\code{signature(object = "TokenizedDocumentView")}:
      Returns a table of the frequencies of the tokens within the
      \code{TokenizedDocumentView}. }

    \item{has}{\code{signature(object = "TokenizedDocumentView",
                               value = "character")}:
      Returns \code{TRUE} if the token \code{value} is contained
      within the \code{TokenizedDocumentView}.}

    \item{tokens}{\code{signature(object = "TokenizedDocumentView")}:
      Return a character vector of the \code{TokenizedDocumentView}'s
      tokens.}

    \item{unique}{\code{signature(x = "TokenizedDocumentView")}:
      Return a character vector of the \code{TokenizedDocumentView}'s
      unique tokens.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tokenize}}
  \code{\linkS4class{View}}
  \code{\linkS4class{DocumentView}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedCorpusView}}
  \code{\linkS4class{RSNLCorpus}}
}

\keyword{classes}
