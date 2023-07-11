\name{RegexTokenizer}
\docType{methods}
\alias{RegexTokenizer}
\alias{RegexTokenizer-methods}
\alias{RegexTokenizer,character-method}
\alias{RegexTokenizer,missing-method}
\alias{RegexTokenizer,Pattern-method}

\title{RegexTokenizer}

\description{
Constructs a \code{\linkS4class{RegexTokenizer}} object.
}

\usage{
RegexTokenizer(pattern, matchToken = TRUE)
}

\arguments{
\item{pattern}{A \code{character} or
  \code{\linkS4class{Pattern}} regular expression.}

\item{matchToken}{A boolean indicating whether the resulting object
  should match tokens (\code{TRUE}) or gaps between tokens
  (\code{FALSE}).}
}

\value{
  An S4 object of class \code{\linkS4class{RegexTokenizer}}.
}

\section{Methods}{
\describe{
  \item{pattern = "character"}{ Construct a
  \code{\linkS4class{RegexTokenizer}} from a string pattern. }

  \item{pattern = "missing"}{ Construct a 
  \code{\linkS4class{RegexTokenizer}} from the default pattern.}

  \item{pattern = "Pattern"}{ Construct a 
  \code{\linkS4class{RegexTokenizer}} from a
  \code{\linkS4class{Pattern}}. }
}}

\examples{
  data(crude)
  crude <- RSNLCorpus(crude)

  # Create as simple space-based tokenizer
  rt <- RegexTokenizer("\\\s+", matchToken=FALSE)

  # Tokenize a corpus
  crude.tok <- tokenize(crude, rt)

  # Tokenize a single document, returning a TokenizedDocumentView
  crude1.tok <- tokenize(crude, rt, index=1)

  # Tokenize a raw document, returning a character vector
  crude1.tok.raw <- tokenize(crude[[1]], rt)

  # Tokenize some raw text. Note that PTBTokenizer is tokenize's default
  tokenize("The men's watches (they all wore watches) ticked in unison.", rt)

}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RegexTokenizer}}
}

\keyword{methods}
