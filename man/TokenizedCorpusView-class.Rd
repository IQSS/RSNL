\name{TokenizedCorpusView-class}
\docType{class}
\alias{TokenizedCorpusView-class}
\alias{TokenizedCorpusView}
\alias{documentTokenMatrix,TokenizedCorpusView,missing-method}
\alias{documentTokenMatrix,TokenizedCorpusView,WeightFunction-method}
\alias{freq,TokenizedCorpusView,character-method}
\alias{freqTable,TokenizedCorpusView-method}
\alias{has,TokenizedCorpusView,character-method}
\alias{unique,TokenizedCorpusView-method}

\title{Class "TokenizedCorpusView"}

\description{A view of a \code{\linkS4class{RSNLCorpus}} as a collection
of \code{\linkS4class{TokenizedDocumentView}} objects.}

\section{Objects from the Class}{
    In general, \code{TokenizedCorpusView} objects should not be
    constructed directly unless you are writing code to extend this
    package. Instead, they can be obtained from an existing 
    \code{\linkS4class{RSNLCorpus}} object by calling
    \code{tokenize(corpus)}.
}

\section{Extends}{
Class \code{"\linkS4class{CorpusView}"}, directly.
Class \code{"\linkS4class{TokenizedView}"}, directly.
Class \code{"\linkS4class{View}"}, by class "CorpusView", distance 2.
}

\section{Methods}{
  \describe{
    \item{documentTokenMatrix}{\code{signature(object = 
                                               "TokenizedCorpusView", 
                                               weight = "missing")}: 
      Returns a \code{\linkS4class{DocumentTermMatrix}} object, generated
      from the given \code{TokenizedCorpusView}.}

    \item{documentTokenMatrix}{\code{signature(object =
                                               "TokenizedCorpusView",
                                               weight = "WeightFunction")}:
      Returns a \code{\linkS4class{DocumentTermMatrix}} object, generated
      from the given \code{TokenizedCorpusView}, weighted by the given
      \code{\linkS4class{WeightFunction}} object.}

    \item{freq}{\code{signature(object = "TokenizedCorpusView",
                                value = "character")}:
      Returns the frequency of the token \code{value} within the
      \code{TokenizedCorpusView}.}

    \item{freqTable}{\code{signature(object = "TokenizedCorpusView")}:
      Returns a table of the frequencies of the tokens within the
      \code{TokenizedCorpusView}. }

    \item{has}{\code{signature(object = "TokenizedCorpusView",
                               value = "character")}:
      Returns \code{TRUE} if the token \code{value} is contained
      within the \code{TokenizedCorpusView}.}
    
    \item{unique}{\code{signature(x = "TokenizedCorpusView")}:
      Return a character vector of the \code{TokenizedCorpusView}'s
      unique tokens.}
  }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tokenize}}
  \code{\linkS4class{View}}
  \code{\linkS4class{CorpusView}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{RSNLCorpus}}
}

\keyword{classes}
