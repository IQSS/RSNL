\name{RegexTokenFilter}
\alias{RegexTokenFilter}

\title{RegexTokenFilter}

\description{
Constructs a \code{\linkS4class{RegexTokenFilter}} object.
}

\usage{
RegexTokenFilter(pattern, negate=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{RegexTokenFilter}}.
}

\arguments{
\item{pattern}{A \code{\linkS4class{Pattern}} or \code{character}
containing a regular expression.}
\item{negate}{A \code{logical} indicating whether to keep the set of tokens
matched by \code{pattern} or the negation of that set.}
}

\details{ A \code{RegexTokenFilter} object encapsulates a regular
expression and uses it to filter a sequence of tokens.  The filter
will retain all tokens that match \code{pattern}, or, if \code{negate}
is set to true, all tokens that do not match \code{pattern}.}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# A tokenized view of crude ignoring all words starting with "c"
pcrude.filt <- filterTokens(pcrude, RegexTokenFilter("^[Cc]", TRUE))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RegexTokenFilter}}
}

\keyword{methods}
