\name{PTBTokenizer}
\alias{PTBTokenizer}

\title{PTBTokenizer}

\description{
Constructs a \code{\linkS4class{PTBTokenizer}} object.
}

\usage{
  PTBTokenizer()
}

\value{
  An S4 object of class \code{\linkS4class{PTBTokenizer}}.
}

\examples{
  data(crude)
  crude <- RSNLCorpus(crude)

  # Tokenize a corpus
  crude.tok <- tokenize(crude, PTBTokenizer())

  # Tokenize a single document, returning a TokenizedDocumentView
  crude1.tok <- tokenize(crude, PTBTokenizer(), index=1)

  # Tokenize a raw document, returning a character vector
  crude1.tok.raw <- tokenize(crude[[1]], PTBTokenizer())

  # Tokenize some raw text. Note that PTBTokenizer is tokenize's default
  tokenize("The men's watches (they all wore watches) ticked in unison.")
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{PTBTokenizer}}
}
