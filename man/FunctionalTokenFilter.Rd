\name{FunctionalTokenFilter}
\alias{FunctionalTokenFilter}

\title{FunctionalTokenFilter}

\description{
Constructs a \code{\linkS4class{FunctionalTokenFilter}} object.
}

\usage{
FunctionalTokenFilter(fun, negate=FALSE, applyToEach=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{FunctionalTokenFilter}}.
}

\arguments{
\item{fun}{A function defining a filtering algorithm.}
\item{negate}{A \code{logical} indicating whether to keep the set of tokens
indicated by \code{func} or the negation of that set.}
\item{applyToEach}{A \code{logical} indicating whether the filtering
function expects a vector of tokens or a single token.}
}

\details{
  A \code{FunctionalTokenFilter} object encapsulates a simple function
  and uses it to filter a sequence of tokens.  The function should
  take a single \code{character} vector (one token per element) as its
  argument and return a \code{logical} vector of the same length as
  its input, indicating which tokens to keep.  If \code{negate} is set
  to true, this returned vector will, instead, indicate which tokens
  to filter out.  If \code{isAppliedToEach} is set to true, the
  function should expect only a single token and return a single
  \code{logical}.
}

\examples{
# A tokenized view of crude ignoring every other word
data(crude)
pcrude <- RSNLCorpus(crude)
oddFilt <- FunctionalTokenFilter(
  fun = function (x) rep(c(TRUE, FALSE), length.out=length(x)))
pcrude.filt <- filterTokens(pcrude, oddFilt)
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{FunctionalTokenFilter}}
}

\keyword{methods}
