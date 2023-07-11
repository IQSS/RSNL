\name{RegexTagFilter}
\alias{RegexTagFilter}

\title{RegexTagFilter}

\description{
Constructs a \code{\linkS4class{RegexTagFilter}} object.
}

\usage{
RegexTagFilter(pattern, negate=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{RegexTagFilter}}.
}

\arguments{
\item{pattern}{A \code{\linkS4class{Pattern}} or \code{character}
containing a regular expression.}
\item{negate}{A \code{logical} indicating whether to keep the set of tokens
with tags matched by \code{pattern} or the negation of that set.}
}

\details{ A \code{RegexTagFilter} object encapsulates a regular
expression and uses it to filter a sequence of tagged tokens.  The filter
will retain all tokens with tags that match \code{pattern}, or, if
\code{negate} is set to true, all tokens with tags that do not match
\code{pattern}.}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# A tokenized view of crude retaining all nouns, but nothing else
pcrude.filt <- filterTokens(tag(pcrude), RegexTagFilter("^NN"))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RegexTagFilter}}
}

\keyword{methods}
