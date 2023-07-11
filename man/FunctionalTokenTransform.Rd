\name{FunctionalTokenTransform}
\alias{FunctionalTokenTransform}

\title{FunctionalTokenTransform}

\description{
Constructs a \code{\linkS4class{FunctionalTokenTransform}} or
\code{\linkS4class{TagSafeFunctionalTokenTransform}} object.
}

\usage{
FunctionalTokenTransform(fun, tagSafe=FALSE, applyToEach=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{FunctionalTokenTransform}}.
}

\arguments{
\item{fun}{A function defining a transformation algorithm.}
\item{tagSafe}{Indicates whether or not the transform is designed to
correctly deal with tagged tokens in \code{linkS4class{Tagged}}
form.}
\item{applyToEach}{A \code{logical} indicating whether the transform
function expects a vector of tokens or a single token.}
}

\details{ 
A \code{\linkS4class{FunctionalTokenTransform}} object encapsulates a
simple function and uses it to transform a sequence of tokens.
\code{\linkS4class{TagSafeFunctionalTokenTransform}} objects are a
specialization of the base class for transforms that are
\code{\linkS4class{Tagged}} aware and can correctly deal with
both simple and tagged token vectors.  The function should take a
single \code{character} vector (one token per element) as its argument
and return a \code{character} vector of post-transform tokens.  If
\code{tagSafe} is set to true the function should also return a
\code{\linkS4class{Tagged}} vector when a
\code{\linkS4class{Tagged}} vector is passed in as its argument.
If \code{isAppliedToEach} is set to true, the function should expect
only a single token and return a length 1 \code{character} or
\code{\linkS4class{Tagged}} vector.
}

\examples{
# A tokenized view of crude in terms of bigrams
data(crude)
pcrude <- RSNLCorpus(crude)
bigTrans <- FunctionalTokenTransform(
  function (x) {
    index <- lapply(1:length(x), function (x) seq(x, x+1))
    sapply(index, function (y) paste(x[y], collapse=" "))
  })

crude.bigram <- transformTokens(pcrude, bigTrans)
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{FunctionalTokenTransform}}
  \code{\linkS4class{TagSafeFunctionalTokenTransform}}
  \code{\link{RegexTokenTransform}}
  \code{\link{TolowerTokenTransform}}
}

\keyword{methods}
