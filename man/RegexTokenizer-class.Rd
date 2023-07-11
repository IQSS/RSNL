\name{RegexTokenizer-class}
\docType{class}
\alias{RegexTokenizer-class}
\alias{matchToken<-,RegexTokenizer,logical-method}
\alias{matchToken,RegexTokenizer-method}
\alias{pattern<-,RegexTokenizer-method}
\alias{pattern,RegexTokenizer-method}
\alias{tokenize,character,RegexTokenizer,missing-method}

\title{Class "RegexTokenizer"}

\description{
  An object encapsulating a regular expression for tokenizing text.
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{RegexTokenizer(pattern, matchToken=TRUE)} where \code{pattern}
  is a \code{character} 1-vector containing a regular expression, or a
  \code{\linkS4class{Pattern}}.  If \code{matchToken} is set to
  \code{TRUE} then \code{pattern} matches tokens, otherwise it matches
  gaps between tokens.
}

\section{Extends}{
Class \code{"\linkS4class{Tokenizer}"}, directly.
}

\section{Methods}{
  \describe{
    \item{matchToken<-}{\code{signature(object = "RegexTokenizer",
                                        value = "logical")}:
      Sets whether the tokenizer matches tokens or gaps between
      tokens. }

    \item{matchToken}{\code{signature(object = "RegexTokenizer")}:
      Returns whether the tokenizer matches tokens (as opposed to gaps
      between tokens). }

    \item{pattern<-}{\code{signature(object = "RegexTokenizer")}:
       Sets the object's regular expression pattern. }

    \item{pattern}{\code{signature(object = "RegexTokenizer")}:
      Returns a \code{\linkS4class{Pattern}} representing the
      tokenizer's regular expression pattern. }

    \item{tokenize}{\code{signature(object = "character",
                                    tokenizer = "RegexTokenizer",
                                    index = "missing")}:
      Applies the \code{RegexTokenizer} to the given \code{character}
      object, returning a \code{character} vector of tokens.}
	 }
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{RegexTokenizer}}
  \code{\link{tokenize}}
  \code{\linkS4class{Tokenizer}}
  \code{\linkS4class{PTBTokenizer}}
  \code{\linkS4class{OffsetTokenizer}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\keyword{classes}
