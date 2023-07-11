\name{PunctTokenFilter}
\alias{PunctTokenFilter}

\title{PunctTokenFilter}

\description{
Constructs a \code{\linkS4class{PunctTokenFilter}} object.
}

\usage{
PunctTokenFilter()
}

\value{
  An S4 object of class \code{\linkS4class{PunctTokenFilter}}.
}

\details{
  A \code{\linkS4class{PunctTokenFilter}} is a specialization of
  \code{\linkS4class{RegexTokenFilter}} that filters out all
  pure-punctuation tokens.
}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# Remove punctuation tokens
pcrude.filt <- filterTokens(pcrude, PunctTokenFilter())
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{PunctTokenFilter}}
}

\keyword{methods}
