\name{StopFilter}
\alias{StopFilter}

\title{StopFilter}

\description{
Constructs a \code{\linkS4class{StopFilter}} object.
}

\usage{
StopFilter(stoplist=stopwords("english"))
}

\value{
  An S4 object of class \code{\linkS4class{StopFilter}}.
}

\arguments{
\item{stoplist}{A \code{character} vector containing a list of tokens
to filter out.}
}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)
tmMap(pcrude, tmTolower)

# A tokenized view of crude eliminating common english words
pcrude.stopped <- filterTokens(pcrude, StopFilter())

# What if there was no gulf war 1?
pcrude.nokuwait <- filterTokens(pcrude, StopFilter("kuwait"))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{StopFilter}}
}

\keyword{methods}
