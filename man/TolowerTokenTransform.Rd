\name{TolowerTokenTransform}
\alias{TolowerTokenTransform}

\title{TolowerTokenTransform}

\description{
Constructs a \code{\linkS4class{TolowerTokenTransform}} object.
}

\usage{
TolowerTokenTransform()
}

\value{
  An S4 object of class \code{\linkS4class{TolowerTokenTransform}}.
}

\details{ A \code{TolowerTokenTransform} object converts all tokens in
a view to lower case.}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)

# Generate an all lowercase tokenized view
pcrude.lc <- transformTokens(pcrude, TolowerTokenTransform())
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{TolowerTokenTransform}}
  \code{\link{FunctionalTokenTransform}}
  \code{\link{TolowerTokenTransform}}
}

\keyword{methods}
