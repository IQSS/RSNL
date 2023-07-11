\name{tokenize}
\alias{tokenize}

\title{tokenize}

\description{
  \code{tokenize} is a generic function that applies
  \code{\linkS4class{Tokenizer}} objects to text, generating a sequence
  of individual tokens, typically words.
}

\usage{
  tokenize(object, tokenizer, index)
  tokenize(object, tokenizer)
  tokenize(object)
  tokenize(object, index=)
}

\value{
  When \code{object} is of type \code{character}, \code{tokenize}
  returns a \code{character} vector of tokens.  When object is of type
  \code{RSNLCorpus}, \code{tokenize} returns a
  \code{\linkS4class{TokenizedCorpusView}}, or, if an \code{index} is
  passed to the function, a
  \code{\linkS4class{TokenizedDocumentView}}.
}

\arguments{
\item{object}{A \code{character} object containing text to be
tokenized, or a \code{\linkS4class{RSNLCorpus}} object.
}
\item{tokenizer}{An object extending the
\code{\linkS4class{Tokenizer}} class.}
\item{index}{A numeric index into a \code{\linkS4class{RSNLCorpus}},
indicating a particular \code{\linkS4class{TextDocument}} object.}
}

\details{The \code{tokenize} method allows users to break character
strings up into a series of substrings, typically words.  By default
\code{character} objects are collapsed into a single long input string
before tokenizing, no matter how many elements they have to begin with.
In addition to basic \code{character} objects, \code{tokenize}
operates on \code{\linkS4class{RSNLCorpus}} objects, generating
\code{\linkS4class{TokenizedCorpusView}}s.  The user may also obtain a
\code{\linkS4class{TokenizedDocumentView}} of a particular member of a
\code{\linkS4class{RSNLCorpus}} by specifying an index in addition to a
\code{RSNLCorpus} when calling \code{tokenize}.  Finally, \code{tokenize}
works by applying a given \code{\linkS4class{Tokenizer}} to the input
object; if the user omits this argument, \code{tokenizer} uses a default
\code{\linkS4class{PTBTokenizer}}.
}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Tokenizer}}
  \code{\linkS4class{PTBTokenizer}}
  \code{\linkS4class{RegexTokenizer}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)

  # Grab a vector of tokens using the default Penn Tree Bank Tokenizer
  d1toks <- tokenize(pcrude[[1]])

  # Generate a tokenized view of the corpus, using a Regex based tokenizer
  tcrude <- tokenize(pcrude, RegexTokenizer())

  # Get a tokenized document view
  td1 <- tokenize(pcrude, index=1)
}

\keyword{methods}
