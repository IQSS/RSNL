\name{filterTokens}
\alias{filterTokens}

\title{filterTokens}

\description{
  \code{filterTokens} is a generic function that applies
  \code{\linkS4class{TokenFilter}} objects to textual objects, or objects
  representing sequences of tokens, generating a sequence of tokens
  with some subset of the original tokens filtered out.
}

\usage{
  filterTokens(object, filter, tokenizer, index)
  filterTokens(object, filter, tokenizer)
  filterTokens(object, filter)
  filterTokens(object)

  filterTokens(object, tokenizer=, index=)
  filterTokens(object, tokenizer=)
  
  filterTokens(object, filter, index=)
  filterTokens(object, index=)
}

\value{
  When \code{object} is of type \code{character}, \code{filterTokens}
  returns a \code{character} vector of filtered tokens.  When object is
  of type \code{RSNLCorpus}, \code{filterTokens} returns a
  \code{\linkS4class{TokenizedCorpusView}}, or, if an \code{index} is
  passed to the function, a
  \code{\linkS4class{TokenizedDocumentView}}.  Finally, when
  object is a sub-type of \code{\linkS4class{TokenizedView}},
  \code{filterTokens} returns a corresponding
  \code{\linkS4class{TokenizedView}}.
}

\arguments{
\item{object}{A \code{character} vector of tokens to filterTokens, a
\code{\linkS4class{RSNLCorpus}} object, or a
\code{\linkS4class{TokenizedView}}.
}
\item{filter}{An object extending the \code{\linkS4class{TokenFilter}}
class.}
\item{tokenizer}{An object extending the \code{\linkS4class{Tokenizer}}
class.}
\item{index}{A numeric index into a \code{\linkS4class{RSNLCorpus}} or
\code{\linkS4class{TokenizedCorpusView}},
indicating a particular \code{\linkS4class{TextDocument}} or
\code{\linkS4class{TokenizedDocumentView}} object.}
}


\details{The \code{filterTokens} methods allows users to filter out
particular tokens from an object (corpus, document, etc) using a given
filter object. By default, \code{filterTokens} assumes that, when
\code{object} is of type \code{character}, it contains a sequence of
tokens to filter, but \code{filterTokens} will apply a
\code{\linkS4class{Tokenizer}} to \code{object} before filtering if
one is provided by the user or if \code{object} is also of type
\code{\linkS4class{TextDocument}}.  In addition to basic
\code{character} objects, \code{filterTokens} operates on
\code{\linkS4class{RSNLCorpus}} objects, generating
\code{\linkS4class{TokenizedCorpusView}}s.  The user may also obtain a
\code{\linkS4class{TokenizedDocumentView}} of a particular member of a
\code{\linkS4class{RSNLCorpus}} by specifying an index in addition to a
\code{RSNLCorpus} when calling \code{filterTokens}.  Furthemore,
\code{object} may also be a \code{\linkS4class{TokenizedView}}, in
which case \code{filterTokens} will return an object of the same type.
\code{filterTokens} works by applying a given
\code{\linkS4class{TokenFilter}} to the input object; if the user
omits this argument, \code{filterTokens} uses a
\code{\linkS4class{StopFilter}} that eliminates common English words.}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{TokenFilter}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)

  # Remove stopwords
  tcrude.stopped <- filterTokens(pcrude)

  # Let's also remove the reuter tag
  tcrude.stopped <- filterTokens(tcrude.stopped,
                                 RegexTokenFilter("reuter", TRUE))

  # Now let's remove punctuation
  tcrude.stopped <- filterTokens(tcrude.stopped,
                                 PunctTokenFilter())

 # Now generate a tagged view that inherits tcrude.stopped's filters.
 tagged <- tag(tcrude.stopped)
}

\keyword{methods}
