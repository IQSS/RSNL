\name{RSNLCorpus-class}
\docType{class}
\alias{RSNLCorpus-class}
\alias{stem,RSNLCorpus,Stemmer,ANY,missing-method}
\alias{stem,RSNLCorpus,Stemmer,ANY,ANY-method}
\alias{tag,RSNLCorpus,Tagger,ANY,missing-method}
\alias{tag,RSNLCorpus,Tagger,ANY,ANY-method}
\alias{tokenize,RSNLCorpus,Tokenizer,ANY-method}
\alias{tokenize,RSNLCorpus,Tokenizer,missing-method}

\title{Class "RSNLCorpus"}
\description{Persistent Corpus objects are wrappers around
\code{\linkS4class{Corpus}} objects from the tm package.  These
objects provide persistent semantics, regardless of the underlying
representation used by the wrapped corpus, and otherwise behave
exactly like the corpus they wrap.  These objects allow users to
generate various ``views'' of corpora.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{RSNLCorpus(object)},
where object is a \code{\linkS4class{Corpus}} object.}

\section{Extends}{
Class \code{"\linkS4class{Corpus}"}, directly.
Class \code{"\linkS4class{list}"}, by class "Corpus", distance 2.
Class \code{"\linkS4class{vector}"}, by class "Corpus", distance 3.
}

\section{Methods}{
  \describe{
    \item{clone}{\code{signature(objet = "RSNLCorpus")}:
      Returns a hard copy of the corpus. }

    \item{stem}{\code{signature(object = "RSNLCorpus", stemmer =
                                "Stemmer", tokenizer = "ANY",
                                index = "missing")}: 
      Apply the given stemmer and, optionally, tokenizer, to the
      corpus and return a \code{\linkS4class{TokenizedCorpusView}}. }

    \item{stem}{\code{signature(object = "RSNLCorpus", stemmer =
                                "Stemmer", tokenizer = "ANY", index = 
                                "numeric")}: 
      Apply the given stemmer and, optionally, tokenizer, to the
      document contained in the corpus at \code{index}
      and return a \code{\linkS4class{TokenizedDocumentView}}. }

    \item{tag}{\code{signature(object = "RSNLCorpus", tagger = "Tagger",
                               tokenizer = "ANY", index = "missing")}: 
      Apply the given tagger and, optionally, tokenizer, to the corpus
      and return a \code{\linkS4class{TaggedCorpusView}}. }

    \item{tag}{\code{signature(object = "RSNLCorpus", tagger = "Tagger",
                               tokenizer = "ANY", index = "numeric")}: 
      Apply the given stemmer, and optionally, tokenizer, to the
      document contained in the corpus at \code{index} and return a
      \code{\linkS4class{TaggedDocumentView}}. }

    \item{tokenize}{\code{signature(object = "RSNLCorpus", 
                                    tokenizer = "Tokenizer", 
                                    index = "missing")}: 
      Apply the given tokenizer to the corpus and return a
      \code{\linkS4class{TokenizedCorpusView}}. }

    \item{tokenize}{\code{signature(object = "RSNLCorpus",
                                    tokenizer = "Tokenizer",
                                    index = "numeric")}:
      Apply the given tokenizer to the document contained in the
      corpus at \code{index} and return a
      \code{\linkS4class{TokenizedDocumentView}}.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\section{NOTE}{RSNLCorpus objects do not currently support the \code{c()}
method (inherited from \code{\linkS4class{Corpus}}) and have not yet
been tested with lazy mapping via \code{tmMap}.}

\seealso{
  \code{\link{RSNLCorpus}}
  \code{\linkS4class{Corpus}}
  \code{\linkS4class{CorpusView}}
}

\keyword{classes}
