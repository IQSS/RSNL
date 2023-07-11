\name{Tagged}
\alias{Tagged}

\title{Tagged}

\description{
Constructs a \code{\linkS4class{Tagged}} object.
}

\usage{
  Tagged(value=character(), tag=character())
}

\value{
  An S4 object of class \code{\linkS4class{Tagged}};
}

\arguments{
\item{value}{A \code{character} vector containing values (tokens) to be
represented by the \code{\linkS4class{Tagged}} object.}
\item{tag}{A \code{character} vector of the same length as
\code{value} containing labels (tags) to associate with the values in
\code{value}.}
}

\examples{
  # Load crude and tag it with part of speech
  data(crude)
  crude<-RSNLCorpus(crude)
  tagged <- tag(crude)

  # Find every word in the corpus that is used as both a noun and a
  # verb in its base form.
  tagged.u <- unique(tagged, what="Tokens")
  nv <- tagged.u[sapply(tagged.u,
    function (u) has(tagged, Tagged(u, "NN")) & has(tagged, Tagged(u, "VB")))]

  # Check out the relative freqs
  sapply(nv,
    function (token) sapply(c("NN", "VB"), 
      function (tag) freq(tagged, Tagged(token, tag))))
}


\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Tagged}}
}

\keyword{methods}
