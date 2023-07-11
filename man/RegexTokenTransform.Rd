\name{RegexTokenTransform}
\alias{RegexTokenTransform}

\title{RegexTokenTransform}

\description{
Constructs a \code{\linkS4class{RegexTokenTransform}} object.
}

\usage{
RegexTokenTransform(pattern, repl="")
}

\value{
  An S4 object of class \code{\linkS4class{RegexTokenTransform}}.
}

\arguments{
\item{pattern}{A \code{\linkS4class{Pattern}} or \code{character}
containing a regular expression.}
\item{repl}{A \code{character} object containing a single string to
replace matches within tokens with.  The string may contain
backslashed escpaes and backreferences.}
}

\details{ A \code{RegexTokenTransform} object encapsulates a regular
expression and uses it to perform substitutions within a sequence of
tokens.  It replaces each token with the string obtained by replacing
the leftmost non-overlapping occurences of \code{pattern} by
\code{repl}.  It will match and replace at most \code{count} times
within a single token, although the value \code{count = 0} indicates
it should match as many times as possible within each token.}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# Replaces honorifics with a single uniform value
pcrude.trans <- transformTokens(pcrude,
  RegexTokenTransform("(Mr|Mrs|Ms|Dr|Miss)", "HONORIFIC"))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RegexTokenTransform}}
  \code{\link{FunctionalTokenTransform}}
  \code{\link{TolowerTokenTransform}}

}

\keyword{methods}
