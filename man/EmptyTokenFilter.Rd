\name{EmptyTokenFilter}
\alias{EmptyTokenFilter}

\title{EmptyTokenFilter}

\description{
Constructs a \code{\linkS4class{EmptyTokenFilter}} object.
}

\usage{
EmptyTokenFilter()
}

\value{
  An S4 object of class \code{\linkS4class{EmptyTokenFilter}}.
}

\details{
  A \code{\linkS4class{EmptyTokenFilter}} is a specialization of
  \code{\linkS4class{RegexTokenFilter}} that filters out all
  empty tokens.
}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# Remove empty tokens
pcrude.filt <- filterTokens(pcrude, EmptyTokenFilter())
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{EmptyTokenFilter}}
}

\keyword{methods}
