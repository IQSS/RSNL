\name{PTBTokenizer-class}
\docType{class}
\alias{PTBTokenizer-class}
\alias{tokenize,character,PTBTokenizer,missing-method}

\title{Class "PTBTokenizer"}
\description{A tokenizer that tokenizes text based on Penn Tree Bank
conventions.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{PTBTokenizer()}.
}

\section{Extends}{
Class \code{"\linkS4class{Tokenizer}"}, directly.
}

\section{Methods}{
  \describe{
    \item{tokenize}{\code{signature(object = "character", tokenizer = "PTBTokenizer", index = "missing")}:
      Applies the \code{PTBTokenizer} to the given \code{character}
      object, returning a \code{character} vector of tokens.}
	 }
}

\details{\code{PTBTokenizer} is RSNL's default
\code{\linkS4class{Tokenizer}} and tokenizes text based on the Penn
Tree Bank's tokenization conventions.  See
http://www.cis.upenn.edu/~treebank/tokenization.html for a description
of the Penn Tree Bank conventions.  This tokenizer is built upon a
Java implementation that is part of the Stanford NLP Group's Part of
Speech Tagger source distribution, available at
http://nlp.stanford.edu/software/tagger.shtml.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{PTBTokenizer}}
  \code{\link{tokenize}}
  \code{\linkS4class{Tokenizer}}
  \code{\linkS4class{RegexTokenizer}}
  \code{\linkS4class{OffsetTokenizer}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
