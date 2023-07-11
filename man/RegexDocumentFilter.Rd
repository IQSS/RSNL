\name{RegexDocumentFilter}
\alias{RegexDocumentFilter}

\title{RegexDocumentFilter}

\description{
Constructs a \code{\linkS4class{RegexDocumentFilter}} object.
}

\usage{
RegexDocumentFilter(pattern, negate=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{RegexDocumentFilter}}.
}

\arguments{
\item{pattern}{A \code{\linkS4class{Pattern}} or \code{character}
representing a regular expression.}
\item{negate}{A \code{logical} indicating whether to keep the set of
document views matched by \code{pattern} or the negation of that set.}
}

\details{ A \code{RegexDocumentFilter} object encapsulates a regular
expression and uses it to filter a collection. of document views.
The filter will retain all views refering to documents (as returned by the
\code{document} method) that match \code{pattern}, or, if
\code{negate} is set to true, all views that do not match
\code{pattern}.}

\examples{
  data(crude)
  tcrude <- tokenize(RSNLCorpus(crude))

  #  or just docs mentioning saudi arabia
  tcrude.saudi <- filterDocuments(tcrude,
    RegexDocumentFilter('[Ss]audi\\s+[Aa]rabia'))
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{RegexDocumentFilter}}
}

\keyword{methods}
