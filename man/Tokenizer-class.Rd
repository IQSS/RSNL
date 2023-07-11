\name{Tokenizer-class}
\docType{class}
\alias{Tokenizer-class}

\title{Class "Tokenizer"}

\description{
  An abstract class representing an object that splits text into
  distinct tokens.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
\describe{
  \item{tokenize}{\code{signature(object = "RSNLCorpus",
                                  tokenizer = "Tokenizer",
                                  index = "numeric")}:
    Apply the given \code{Tokenizer} to the given
    \code{\linkS4class{RSNLCorpus}}, generating a
    \code{\linkS4class{TokenizedDocumentView}} from the document at
    the given \code{index}.}

  \item{tokenize}{\code{signature(object = "RSNLCorpus",
                                  tokenizer = "Tokenizer",
                                  index = "missing")}:
    Apply the given \code{Tokenizer} to the given
    \code{\linkS4class{RSNLCorpus}}, generating a
    \code{\linkS4class{TokenizedCorpusView}}.}
  }
}

\details{See \code{\link{tokenize}} for more details on applying
\code{Tokenizer} objects.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tokenize}}
  \code{\linkS4class{RegexTokenizer}}
  \code{\linkS4class{OffsetTokenizer}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
