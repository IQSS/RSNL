\name{FunctionalDocumentFilter}
\alias{FunctionalDocumentFilter}

\title{FunctionalDocumentFilter}

\description{
Constructs a \code{\linkS4class{FunctionalDocumentFilter}} object.
}

\usage{
FunctionalDocumentFilter(fun, negate=FALSE, applyToEach=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{FunctionalDocumentFilter}}.
}

\arguments{
\item{fun}{A function defining a filtering algorithm.}
\item{negate}{A \code{logical} indicating whether to keep the set of
document views indicated by \code{func} or the negation of that set.}
\item{applyToEach}{A \code{logical} indicating whether the filtering
function expects a \code{list} of document views or a single view.}
}

\details{
  A \code{FunctionalDocumentFilter} object encapsulates a simple function
  and uses it to filter a collection of document views.  The function should
  take a list of \code{\linkS4class{DocumentView}}s as its only
  argument and return a \code{logical} vector of the same length as
  its input, indicating which views to keep.  If \code{negate} is set
  to true, this returned vector will, instead, indicate which document
  views to filter out.  If \code{isAppliedToEach} is set to true, the
  function should expect only a single
  \code{\linkS4class{DocumentView}} and return a single
  \code{logical} value.
}

\examples{
  # A tokenized view of crude
  data(crude)
  crude<-RSNLCorpus(crude)
  tcrude <- tokenize(crude)

  # Let's just look at docs with Authors
  tcrude.author <- filterDocuments(tcrude, FunctionalDocumentFilter(
    function (x) slot(document(x), "Author") != "", applyToEach=TRUE))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{FunctionalDocumentFilter}}
}

\keyword{methods}
