\name{OffsetTokenizer-class}
\docType{class}
\alias{OffsetTokenizer-class}
\alias{offsets<-,OffsetTokenizer-method}
\alias{offsets,OffsetTokenizer-method}
\alias{tokenize,character,OffsetTokenizer,missing-method}

\title{Class "OffsetTokenizer"}

\description{An object encapsulating a set of offsets at which to
split a string into tokens.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{OffsetTokenizer(offset)} where offset is an \code{integer}
vector specifying indices at which to split a string.
}

\section{Extends}{
Class \code{"\linkS4class{Tokenizer}"}, directly.
}

\section{Methods}{
  \describe{
    \item{offsets<-}{\code{signature(object = "OffsetTokenizer")}:
      Sets the vector of offsets used by the tokenizer. }
    \item{offsets}{\code{signature(object = "OffsetTokenizer")}:
      Returns the vector of offsets used by the tokenizer. }
    \item{tokenize}{\code{signature(object = "character",
                                    tokenizer = "OffsetTokenizer",
                                    index = "missing")}:
      Applies the \code{OffsetTokenizer} to the given \code{character}
      object, returning a \code{character} vector of tokens.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{OffsetTokenizer}}
  \code{\link{tokenize}}
  \code{\linkS4class{Tokenizer}}
  \code{\linkS4class{RegexTokenizer}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
