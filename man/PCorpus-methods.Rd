\name{RSNLCorpus}
\docType{methods}
\alias{RSNLCorpus}
\alias{RSNLCorpus-methods}
\alias{RSNLCorpus,Corpus-method}
\title{RSNLCorpus}
\description{
Constructs a wrapper around a \code{\linkS4class{Corpus}}, providing a
compatibility layer between the tm and RSNL packages.
}

\usage{\S4method{RSNLCorpus}{Corpus}(object)}

\arguments{
  \item{object}{A \code{\linkS4class{Corpus}} object.}
}


\value{
  An S4 object of class \code{\linkS4class{RSNLCorpus}}, which extends the
  class \code{\linkS4class{Corpus}} containing a collection of text
  documents.
}

\examples{
  data(crude)
  pcrude <- RSNLCorpus(crude)
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RSNLCorpus}}
  \code{\linkS4class{Corpus}}
}

\keyword{methods}
