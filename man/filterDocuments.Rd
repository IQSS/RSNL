\name{filterDocuments}
\docType{methods}
\alias{filterDocuments}

\title{filterDocuments}

\description{
  \code{filterDocuments} is a generic function that applies
  \code{\linkS4class{DocumentFilter}} objects to
  \code{\linkS4class{CorpusView}}s or lists of
  \code{\linkS4class{DocumentView}}s, generating representations of
  document collections with some subset of the documents filtered, or
  hidden from view.
}

\usage{
  filterDocuments(object, filter)
}

\value{
  When \code{object} is list of \code{\linkS4class{DocumentView}}
  objects, \code{filterDocuments} returns a similar list with certain
  views filtered out.  When object is some
  \code{\linkS4class{CorpusView}} sub-type it returns a filtered view
  of the same type.
}

\arguments{
  \item{object}{A \code{list} of \code{\linkS4class{DocumentView}}
  objects or a \code{\linkS4class{CorpusView}} object to filter.}

  \item{filter}{A \code{\linkS4class{DocumentFilter}} to apply to
  \code{object}.}
}

\details{
  The \code{filterDocuments} method allows the user to
  non-destructively filter a corpus, generating a view of that corpus
  with certain documents hidden from view.  Currently, one cannot
  invoke \code{filterDocuments} directly on a
  \code{\linkS4class{RSNLCorpus}} object, although this may change in
  later releases.  Rather, one may only invoke \code{filterDocuments}
  on a view of a corpus (or list of views), generating a newly
  filtered view of the same type.
}

\seealso{
  \code{\linkS4class{DocumentFilter}}
  \code{\linkS4class{FunctionalDocumentFilter}}
  \code{\linkS4class{RegexDocumentFilter}}
  \code{\linkS4class{CorpusView}}
}

\examples{
  # Load the crude oil reuters corpus and tokenize
  data(crude)
  tcrude <- tokenize(RSNLCorpus(crude))
  
  # Let's just look at docs with Authors
  tcrude.author <- filterDocuments(tcrude, FunctionalDocumentFilter(
    function (x) slot(document(x), "Author") != "", applyToEach=TRUE))

  #  or just docs mentioning saudi arabia
  tcrude.saudi <- filterDocuments(tcrude,
    RegexDocumentFilter('[Ss]audi\\s+[Aa]rabia'))


}

\keyword{methods}
