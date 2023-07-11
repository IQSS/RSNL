\name{OffsetTokenizer}
\alias{OffsetTokenizer}

\title{OffsetTokenizer}

\description{
Constructs an \code{\linkS4class{OffsetTokenizer}} object.
}

\usage{
OffsetTokenizer(offset)
}

\value{
  An S4 object of class \code{\linkS4class{OffsetTokenizer}}.
}

\arguments{
\item{offset}{A \code{integer} vector specifying the indices at which
to split tokens.}
}

\examples{
  data(crude)
  tok1 <- tokenize(crude[[1]], OffsetTokenizer(c(1, 22)))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{OffsetTokenizer}}
}

\keyword{methods}
