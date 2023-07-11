\name{SnowballStemmer}
\alias{SnowballStemmer}

\title{SnowballStemmer}

\description{
Constructs an \code{\linkS4class{SnowballStemmer}} object.
}

\usage{
SnowballStemmer(language = "english")
}

\value{
  An S4 object of class \code{\linkS4class{SnowballStemmer}}.
}

\arguments{
\item{language}{A \code{character} string specifying the language the
stemmer should use.}
}

\examples{
## Snowball stemmer with english is the default
ss.en <- SnowballStemmer("english")
stem("Two eggs, please.", ss.en)

## Other languages may be supported, see the Rstem
## documentation for more information
ss.es <- SnowballStemmer("spanish")
stem("Dos huevos, por favor.", ss.es)
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{SnowballStemmer}}
}

\keyword{methods}
