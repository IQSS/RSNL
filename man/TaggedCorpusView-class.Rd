\name{TaggedCorpusView-class}
\docType{class}
\alias{TaggedCorpusView-class}
\alias{TaggedCorpusView}
\alias{documentTaggedTokenMatrix,TaggedCorpusView,missing-method}
\alias{documentTaggedTokenMatrix,TaggedCorpusView,WeightFunction-method}
\alias{documentTagMatrix,TaggedCorpusView,missing-method}
\alias{documentTagMatrix,TaggedCorpusView,WeightFunction-method}
\alias{freq,TaggedCorpusView,character-method}
\alias{freq,TaggedCorpusView,Tagged-method}
\alias{freqTable,TaggedCorpusView-method}
\alias{has,TaggedCorpusView,character-method}
\alias{has,TaggedCorpusView,Tagged-method}
\alias{unique,TaggedCorpusView-method}

\title{Class "TaggedCorpusView"}

\description{
  A view of a \code{\linkS4class{RSNLCorpus}} as a collection of
  \code{\linkS4class{TaggedDocumentView}} objects.
}

\section{Objects from the Class}{
    In general, \code{TaggedCorpusView} objects should not be
    constructed directly unless you are writing code to extend this
    package. Instead, they can be obtained from an existing 
    \code{\linkS4class{RSNLCorpus}} object by calling
    \code{tag(corpus)}.
}

\section{Extends}{
Class \code{"\linkS4class{TaggedView}"}, directly.
Class \code{"\linkS4class{TokenizedCorpusView}"}, directly.
Class \code{"\linkS4class{CorpusView}"}, by class
"TokenizedCorpusView", distance 2.
Class \code{"\linkS4class{TokenizedView}"}, by class
"TokenizedCorpusView", distance 2.
Class \code{"\linkS4class{View}"}, by class "TokenizedCorpusView",
distance 3.
}

\section{Methods}{
  \describe{
    \item{documentTaggedTokenMatrix}{\code{signature(object = 
                            "TaggedCorpusView", weight = "missing")}:
      Returns a \code{\linkS4class{DocumentTermMatrix}} object from the
      \code{TaggedCorpusView} where the terms are token-tag pairs of
      of the form "token/tag". }

    \item{documentTaggedTokenMatrix}{\code{signature(object = 
                        "TaggedCorpusView", weight = "WeightFunction")}:
      Returns a \code{\linkS4class{DocumentTermMatrix}} object from the
      \code{TaggedCorpusView} where the terms are token-tag pairs of
      of the form "token/tag", weighted by the given
      \code{\linkS4class{WeightFunction}} object. }

    \item{documentTagMatrix}{\code{signature(object = "TaggedCorpusView",
                                             weight = "missing")}:
      Returns a \code{\linkS4class{DocumentTermMatrix}} object of
      document-level tag frequencies from the \code{TaggedCorpusView}.  }

    \item{documentTagMatrix}{\code{signature(object = "TaggedCorpusView",
                                             weight = "WeightFunction")}:
      Returns a \code{\linkS4class{DocumentTermMatrix}} object of
      document-level tag frequencies from the \code{TaggedCorpusView},
      weighted by the given \code{\linkS4class{WeightFunction}}
      object. }

    \item{freq}{\code{signature(object = "TaggedCorpusView",
                                value = "character")}:
      By default, returns the frequency of the token-tag pair
      \code{value} within the \code{TaggedCorpusView}.  The pair
      should be written in the form "token/tag".  The method can also
      return the frequency of particular tokens or tags within the
      view.  The optional argument \code{what = c("TaggedTokens",
      "Tokens", "Tags")} indicates where the method should search for
      \code{value}.}
    
    \item{freq}{\code{signature(object = "TaggedCorpusView",
                                value = "Tagged")}:
      A specialization of \code{freq} for \code{\linkS4class{Tagged}}
      objects.  Returns the frequency of the tagged token, token, or
      tag represented by \code{value}, depending on the value of the
      optional argument \code{what = c("TaggedTokens", "Tokens",
      "Tags")}.}

    \item{freqTable}{\code{signature(object = "TaggedCorpusView")}:
      Returns a table of the frequencies of token-tag pairs, tokens,
      or tags within the \code{TaggedCorpusView}.  The method can also
      return a frequency table of tokens or tags.  The optional
      \code{what = c("TaggedTokens", "Tokens", "Tags")} indicates
      what type of frequency table the method should return.}

    \item{has}{\code{signature(object = "TaggedCorpusView",
                               value = "character")}:
      Returns \code{TRUE} if the token-tag pair \code{value} is
      contained within the \code{TaggedCorpusView}. The method can
      also indicate whether \code{object} contains particular tokens
      or tags.  The optional \code{what = c("TaggedTokens", "Tokens",
      "Tags")} indicates where the method should search for
      \code{value}.}

    \item{has}{\code{signature(object = "TaggedCorpusView",
                               value = "Tagged")}:
      A specialization of \code{has} for \code{\linkS4class{Tagged}}
      objects.  Returns \code{TRUE} if the tagged token, token, or
      tag represented by \code{value} is contained within the
      \code{TaggedCorpusView}, depending on the value of the
      optional argument \code{what = c("TaggedTokens", "Tokens",
      "Tags")}.}

    \item{unique}{\code{signature(x = "TaggedCorpusView")}:
      Returns a character vector of the \code{TaggedCorpusView}'s
      unique token-tag pairs by default.  The method can also return
      unique tokens or tags.  The optional \code{what =
      c("TaggedTokens", "Tokens", "Tags")} indicates what set of
      unique values to return.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tag}}
  \code{\linkS4class{CorpusView}}
  \code{\linkS4class{TaggedView}}
  \code{\linkS4class{TaggedDocumentView}}
  \code{\linkS4class{Tagged}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedCorpusView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{RSNLCorpus}}
}

\keyword{classes}
