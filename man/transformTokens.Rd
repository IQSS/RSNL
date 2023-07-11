\name{transformTokens}
\alias{transformTokens}

\title{transformTokens}

\description{
  \code{transformTokens} is a generic function that applies
  \code{\linkS4class{TokenTransform}} objects to textual objects, or objects
  representing sequences of tokens, generating a sequence of tokens
  with some or all of the original tokens transformed in some way.
  The returned vector need not be the same length as the input.
}

\usage{
  transformTokens(object, transform, tokenizer, index)
  transformTokens(object, transform, tokenizer)
  transformTokens(object, transform)
  transformTokens(object)

  transformTokens(object, tokenizer=, index=)
  transformTokens(object, tokenizer=)
  
  transformTokens(object, transform, index=)
  transformTokens(object, index=)
}

\value{
  When \code{object} is of type \code{character}, \code{transformTokens}
  returns a \code{character} vector of transformed tokens.  When object is
  of type \code{RSNLCorpus}, \code{transformTokens} returns a
  \code{\linkS4class{TokenizedCorpusView}}, or, if an \code{index} is
  passed to the function, a
  \code{\linkS4class{TokenizedDocumentView}}.  Finally, when
  object is a sub-type of \code{\linkS4class{TokenizedView}},
  \code{transformTokens} returns a corresponding
  \code{\linkS4class{TokenizedView}}.
}

\arguments{
\item{object}{A \code{character} vector of tokens to transformTokens, a
\code{\linkS4class{RSNLCorpus}} object, or a
\code{\linkS4class{TokenizedView}}.
}
\item{transform}{An object extending the \code{\linkS4class{TokenTransform}}
class.}
\item{tokenizer}{An object extending the \code{\linkS4class{Tokenizer}}
class.}
\item{index}{A numeric index into a \code{\linkS4class{RSNLCorpus}} or
\code{\linkS4class{TokenizedCorpusView}},
indicating a particular \code{\linkS4class{TextDocument}} or
\code{\linkS4class{TokenizedDocumentView}} object.}
}


\details{The \code{transformTokens} methods allows users to modify
particular tokens from an object (corpus, document, etc) using a given
transform object. By default, \code{transformTokens} assumes that, when
\code{object} is of type \code{character}, it contains a sequence of
tokens to transform, but \code{transformTokens} will apply a
\code{\linkS4class{Tokenizer}} to \code{object} before transforming if
one is provided by the user or if \code{object} is also of type
\code{\linkS4class{TextDocument}}.  In addition to basic
\code{character} objects, \code{transformTokens} operates on
\code{\linkS4class{RSNLCorpus}} objects, generating
\code{\linkS4class{TokenizedCorpusView}}s.  The user may also obtain a
\code{\linkS4class{TokenizedDocumentView}} of a particular member of a
\code{\linkS4class{RSNLCorpus}} by specifying an index in addition to a
\code{RSNLCorpus} when calling \code{transformTokens}.  Furthemore,
\code{object} may also be a \code{\linkS4class{TokenizedView}}, in
which case \code{transformTokens} will return an object of the same type.
\code{transformTokens} works by applying a given
\code{\linkS4class{TokenTransform}} to the input object; if the user
omits this argument, \code{transformTokens} uses a
\code{\linkS4class{TolowerTokenTransform}} that converts tokens to all
lowercase letters.}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{TokenTransform}}
  \code{\linkS4class{TokenizedView}}
  \code{\linkS4class{TokenizedDocumentView}}
  \code{\linkS4class{TokenizedCorpusView}}
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)

  # lowercase tokens
  tcrude.lower <- transformTokens(pcrude)

  # Word replacement
  tcrude.repl <- transformTokens(tcrude.lower,
    RegexTokenTransform("minister|sheikh|analysts?|traders?", "PERSON"))

  # Stemming (you could use the stem method here too).
  tcrude.stemmed <- transformTokens(tcrude.lower, SnowballStemmer())

}

\keyword{methods}
