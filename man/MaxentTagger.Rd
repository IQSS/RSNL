\name{MaxentTagger}
\alias{MaxentTagger}

\title{MaxentTagger}

\description{
Constructs a \code{\linkS4class{MaxentTagger}} object.
}

\usage{
MaxentTagger(language = NULL)
}

\value{
  An S4 object of class \code{\linkS4class{MaxentTagger}}.
}

\arguments{
\item{language}{A \code{character} string specifying the langauge the
tagger should use.  Current languages enclude Arabic, Chinese,
English, and German.}
}

\details{A \code{MaxentTagger} object provides an interface to Maximum
Entropy based part-of-speech tagging engines developed by the Stanford
Natural Language Processing Group.  The system initializes a given
language engine the first time one constructs a
\code{\linkS4class{MaxentTagger}}.  Further instantiations of
\code{\linkS4class{MaxentTagger}} objects within a given R session
will all use the same underlying engine and it is not possible to use
multiple language engines within a single session.  Calling
\code{MaxentTagger()} with no arguments will intialize the English
language engine initially; after initialization subsequent no-argument
calls to \code{MaxentTagger()} will return objects providing
interfaces to the intialized engine, regardless of the underlying
language.}

\examples{
  data(crude)
  pcrude<-RSNLCorpus(crude)

  # Tag a corpus in English
  pcrude.pos <- tag(pcrude, MaxentTagger("english"))

  # Tag a line of text
  tag("The rabbit walked home.", MaxentTagger())
  
  # Error: tagger engine already initialized for English
  \dontrun{tag("Der Hase geht nach Hause.", MaxentTagger("german"))}
}

\references{
\item{}{Kristina Toutanova and Christopher D. Manning. 2000.
Enriching the Knowledge Sources Used in a Maximum Entropy
Part-of-Speech Tagger. In Proceedings of the Joint SIGDAT Conference
on Empirical Methods in Natural Language Processing and Very Large
Corpora (EMNLP/VLC-2000), pp. 63-70.}
\item{}{Kristina Toutanova, Dan Klein, Christopher Manning, and Yoram
Singer. 2003. Feature-Rich Part-of-Speech Tagging with a Cyclic
Dependency Network. In Proceedings of HLT-NAACL 2003, pp. 252-259.}
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{MaxentTagger}}
}

\keyword{methods}
