\name{FunctionalTagFilter}
\alias{FunctionalTagFilter}

\title{FunctionalTagFilter}

\description{
Constructs a \code{\linkS4class{FunctionalTagFilter}} object.
}

\usage{
FunctionalTagFilter(fun, negate=FALSE, applyToEach=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{FunctionalTagFilter}}.
}

\arguments{
\item{fun}{A function defining a filtering algorithm.}
\item{negate}{A \code{logical} indicating whether to keep the set of tokens
indicated by \code{func} or the negation of that set.}
\item{applyToEach}{A \code{logical} indicating whether the filtering
function expects a vector of tagged tokens or a single tagged token.}
}

\details{ A \code{FunctionalTagFilter} object encapsulates a simple
function and uses it to filter a sequence of tokens, based on the
values of associated tags.  The function should take a single
\code{\linkS4class{Tagged}} vector (one tagged token per element) as
its argument and return a \code{logical} vector of the same length as
its input, indicating which tokens to keep.  If \code{negate} is set
to true, this returned vector will, instead, indicate which tokens to
filter out.  If \code{isAppliedToEach} is set to true, the function
should expect only a single token and return a single \code{logical}.
\code{FunctionalTokenFilter} objects only work with tagged types and
cannot be used to filter objects that do not extend the
\code{\linkS4class{TaggedView}} or \code{\linkS4class{Tagged}}
classes.  }


\examples{
# A view of crude retaining singular proper nouns
data(crude)
pcrude <- RSNLCorpus(crude)
tagged <- tag(pcrude)

pcrude.filt <- filterTokens(tagged, 
  FunctionalTagFilter(fun=function (x) x == "NNP"))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{FunctionalTagFilter}}
}

\keyword{methods}
