\name{TokenDocFreqFilter}
\alias{TokenDocFreqFilter}

\title{TokenDocFreqFilter}

\description{
Constructs a \code{\linkS4class{TokenDocFreqFilter}} object.
}

\usage{
TokenDocFreqFilter(view, min=1, max=Inf)
}

\value{
  An S4 object of class \code{\linkS4class{TokenDocFreqFilter}}.
}

\arguments{
\item{view}{A \code{\linkS4class{TokenizedCorpusView}} from which to
generate document-token frequencies.}
\item{min}{Must be at single \code{numeric} value of at least zero.
If less than one this specifies the minimum percentage of corpus
documents a token must appear in to survive the filter.  If greater
than or equal to one it indicates the minimum number of documents that
the token must appear in to survive the filter.}
\item{max}{A single \code{numeric} value at least as large as
\code{min}.  If \code{min} is less than one \code{max} is taken to
represent the maximum percentage of corpus documents that a token can
appear in and survive the filter (values of \code{max} greater than 1
are truncated to 1 in this case).  Otherwise, \code{max} represents
the maximum number of documents that a token can appear in and survive
the filter.}
}

\details{
  One should generally only apply a \code{TokenDocFreqFilter} to the
  \code{\linkS4class{TokenizedCorpusView}} that was used to create the
  object.  While doing otherwise is technically legal, the results are
  undefined in many cases.
}

\examples{
data(crude)
pcrude <- RSNLCorpus(crude)
tcrude <- tokenize(pcrude)

# Create a filter that eliminates tokens that don't appear in at
# least 20 percent of the docs
filt <- TokenDocFreqFilter(tcrude, .5)

sort(freqTable(filterTokens(tcrude, filt)), dec=TRUE)

}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{TokenDocFreqFilter}}
}

\keyword{methods}
