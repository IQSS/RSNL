\name{Tagger-class}
\docType{class}
\alias{Tagger-class}
\alias{Tagger}
\alias{tag,character,Tagger,missing,missing-method}
\alias{tag,character,Tagger,Tokenizer,missing-method}
\alias{tag,TokenizedDocumentView,Tagger,missing,missing-method}
\alias{tag,TokenizedCorpusView,Tagger,missing,missing-method}
\alias{tag,TokenizedCorpusView,Tagger,missing,numeric-method}
\alias{tag,RSNLCorpus,Tagger,ANY,numeric-method}

\title{Class "Tagger"}

\description{
  An abstract class for representing Taggers, which apply labels, such
as part of speech tags, to tokens.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{tag}{\code{signature(object = "character", tagger = "Tagger",
                               tokenizer = "missing", index = "missing")}:
      Apply the given tagger to a \code{character} object containing a
      sequence of tokens, returning a \code{character} vector of tags.  }

    \item{tag}{\code{signature(object = "character", tagger = "Tagger",
                               tokenizer = "Tokenizer", index = "missing")}:
      Apply the given tagger to a \code{character} object after first
      tokenizing the object with the given tokenizer and return a
      \code{character} vector of tags. }

    \item{tag}{\code{signature(object = "TokenizedDocumentView",
                               tagger = "Tagger", tokenizer = "missing",
                               index = "missing")}:
      Apply the given tagger to a
      \code{\linkS4class{TokenizedDocumentView}} and return a
      \code{\linkS4class{TaggedDocumentView}}. }

    \item{tag}{\code{signature(object = "TokenizedCorpusView",
                               tagger = "Tagger", tokenizer = "missing",
                               index = "missing")}:
      Apply the given tagger to a
      \code{\linkS4class{TokenizedCorpusView}} and return a
      \code{\linkS4class{TaggedCorpusView}}. }

    \item{tag}{\code{signature(object = "TokenizedCorpusView",
                               tagger = "Tagger", tokenizer = "missing",
                               index = "numeric")}:
      Apply the given tagger to a
      \code{\linkS4class{TokenizedDocumentView}} at \code{index}
      within the \code{\linkS4class{TokenizedCorpusView}} and return a
      \code{\linkS4class{TaggedDocumentView}}. }

    \item{tag}{\code{signature(object = "RSNLCorpus", tagger = "Tagger",
                               index = "missing")}:
      Apply the given tagger to the
      \code{\linkS4class{RSNLCorpus}} and return a
      \code{\linkS4class{TaggedCorpusView}}. }

    \item{tag}{\code{signature(object = "RSNLCorpus", tagger = "Tagger",
                               index = "numeric")}:
      Apply the given stemmer to the
      \code{\linkS4class{TextDocument}} at \code{index}
      within the \code{\linkS4class{RSNLCorpus}} and return a
      \code{\linkS4class{TaggedDocumentView}}. }
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tag}}
  \code{\linkS4class{MaxentTagger}}
  \code{\linkS4class{NamedEntityTagger}}
  \code{\linkS4class{TaggedView}}
  \code{\linkS4class{TaggedDocumentView}}
  \code{\linkS4class{TaggedCorpusView}}
  \code{\linkS4class{Tagged}}
}

\keyword{classes}
