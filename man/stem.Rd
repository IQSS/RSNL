\name{stem}
\alias{stem}

\title{stem}

\description{
  \code{stem} is a generic function that applies
  \code{\linkS4class{Stemmer}} objects to textual objects, or objects
  representing sequences of tokens, generating a sequence of "stemmed"
  tokens, or words that have been reduced to their roots.

  The \code{stem} method serves as an alias for
  \code{\link{transformTokens}} that only works for
  \code{\linkS4class{Stemmer}} objects.
}

\usage{
  stem(object, stemmer, tokenizer, index)
  stem(object, stemmer, tokenizer)
  stem(object, stemmer)
  stem(object)

  stem(object, tokenizer=, index=)
  stem(object, tokenizer=)
  
  stem(object, stemmer, index=)
  stem(object, index=)
}

\value{
  When \code{object} is of type \code{character}, \code{stem}
  returns a \code{character} vector of stemmed tokens.  When object is
  of type \code{RSNLCorpus}, \code{stem} returns a
  \code{\linkS4class{TokenizedCorpusView}}, or, if an \code{index} is
  passed to the function, a
  \code{\linkS4class{TokenizedDocumentView}}.  Finally, when object is of
  type \code{\linkS4class{TokenizedView}}, \code{stem} returns an
  object of the same type.
}

\arguments{
\item{object}{A \code{character} vector of tokens to stem, a
\code{\linkS4class{RSNLCorpus}} object, or a
\code{\linkS4class{TokenizedView}}.
}
\item{stemmer}{An object extending the \code{\linkS4class{Stemmer}}
class.}
\item{tokenizer}{An object extending the \code{\linkS4class{Tokenizer}}
class.}
\item{index}{A numeric index into a \code{\linkS4class{RSNLCorpus}} or
\code{\linkS4class{TokenizedCorpusView}},
indicating a particular \code{\linkS4class{TextDocument}} or
\code{\linkS4class{TokenizedDocumentView}} object.}
}

\details{The \code{stem} method allows users to reduce word tokens to
their roots, by removing common endings.  By default, \code{stem}
assumes that, when \code{object} is of type \code{character}, it
contains a sequence of tokens to stem, but \code{stem} will apply a
\code{\linkS4class{Tokenizer}} to \code{object} before stemming if one
is provided by the user or if \code{object} is also of type
\code{\linkS4class{TextDocument}}.  In addition to basic
\code{character} objects, \code{stem} operates on
\code{\linkS4class{RSNLCorpus}} objects, generating
\code{\linkS4class{TokenizedCorpusView}}s.  The user may also obtain a
\code{\linkS4class{TokenizedDocumentView}} of a particular member of a
\code{\linkS4class{RSNLCorpus}} by specifying an index in addition to a
\code{RSNLCorpus} when calling \code{stem}.  Furthemore, \code{object}
may also be a \code{\linkS4class{TokenizedView}}, in which case
\code{stem} will return a similarly typed object.  Finally,
\code{stem} works by applying a given \code{\linkS4class{Stemmer}} to
the input object; if the user omits this argument, \code{stem} uses an
English-language \code{\linkS4class{SnowballStemmer}}.  }


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Stemmer}}
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)

  # Grab a vector of stemmed tokens
  d1stem <- stem(pcrude[[1]])

  # Generate a stemmed view of the corpus
  scrude <- stem(pcrude, SnowballStemmer("english"))

  # Get a stemmed document view
  td1 <- stem(pcrude, index=1)

  # Generate a stemmed view from a tokenized one
  tcrude <- tokenize(pcrude)
  scrude2 <- stem(tcrude)
}

\keyword{methods}
