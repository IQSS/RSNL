\name{tag}
\alias{tag}

\title{tag}

\description{
  \code{tag} is a generic function that applies
  \code{\linkS4class{Tagger}} objects to textual objects, or objects
  representing sequences of tokens, generating a sequence of
  corresponding labels, or tags.
}

\usage{
  tag(object, tagger, tokenizer, index)
  tag(object, tagger, tokenizer)
  tag(object, tagger)
  tag(object)

  tag(object, tokenizer=, index=)
  tag(object, tokenizer=)
  
  tag(object, tagger, index=)
  tag(object, index=)
}

\value{
  When \code{object} is of type \code{character}, \code{tag}
  returns a \code{character} vector of tags.  When object is of type
  \code{RSNLCorpus}, \code{tag} returns a
  \code{\linkS4class{TaggedCorpusView}}, or, if an \code{index} is
  passed to the function, a
  \code{\linkS4class{TaggedDocumentView}}.  Finally, when object is of
  type \code{\linkS4class{TokenizedView}}, \code{tag} returns a
  corresponding \code{\linkS4class{TaggedView}}.
}

\arguments{
\item{object}{A \code{character} vector of tokens to tag, a
\code{\linkS4class{RSNLCorpus}} object, or a
\code{\linkS4class{TokenizedView}}.
}
\item{tagger}{An object extending the \code{\linkS4class{Tagger}}
\item{tokenizer}{An object extending the \code{\linkS4class{Tokenizer}}
class.}
\item{index}{A numeric index into a \code{\linkS4class{RSNLCorpus}} or
\code{\linkS4class{TokenizedCorpusView}},
indicating a particular \code{\linkS4class{TextDocument}} or
\code{\linkS4class{TokenizedDocumentView}} object.}
}
}

\details{The \code{tag} method allows users to label string tokens
with labels, or tags.  By default, \code{tag} assumes that, when
\code{object} is of type \code{character}, it contains a sequence of
tokens to tag, but \code{tag} will apply a
\code{\linkS4class{Tokenizer}} to \code{object} before tagging if one
is provided by the user, if \code{object} is a scalar \code{character}
object, or if \code{object} is of type
\code{\linkS4class{TextDocument}}.  In addition to basic
\code{character} objects, \code{tag} operates on
\code{\linkS4class{RSNLCorpus}} objects, generating
\code{\linkS4class{TaggedCorpusView}}s.  The user may also obtain a
\code{\linkS4class{TaggedDocumentView}} of a particular member of a
\code{\linkS4class{RSNLCorpus}} by specifying an index in addition to a
\code{RSNLCorpus} when calling \code{tag}.  Furthemore, \code{object} may
also be a \code{\linkS4class{TokenizedView}}, in which case \code{tag}
will return a corresponding \code{\linkS4class{TaggedView}}.  Finally,
\code{tag} works by applying a given \code{\linkS4class{Tagger}} to
the input object; if the user omits this argument, \code{tag} uses a
\code{\linkS4class{MaxentTagger}}.  }


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Tagger}}
  \code{\linkS4class{MaxentTagger}}
  \code{\linkS4class{NamedEntityTagger}}
  \code{\linkS4class{TaggedView}}
  \code{\linkS4class{TaggedDocumentView}}
  \code{\linkS4class{TaggedCorpusView}}
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)

  # Tag a document with part of speech, getting a vector of type Tagged
  crude1.pos <- tag(pcrude[[1]], MaxentTagger("english"))

  # Generate a tagged view of the corpus, based on regex tokenization
  crude.pos <- tag(pcrude, MaxentTagger(), RegexTokenizer())

  # Get a document view tagged by the named entity recognizer
  crude1.tdv.ner <- tag(pcrude, NamedEntityTagger(), index=1)

  # Generate a tagged view from a tokenized one
  tcrude <- tokenize(pcrude)
  tagcrude2 <- tag(tcrude)
}

\keyword{methods}
