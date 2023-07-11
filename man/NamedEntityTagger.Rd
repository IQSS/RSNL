\name{NamedEntityTagger}
\alias{NamedEntityTagger}

\title{NamedEntityTagger}

\description{
Constructs a \code{\linkS4class{NamedEntityTagger}} object.
}

\usage{
NamedEntityTagger(model = "3-class", distsim=FALSE)
}

\value{
  An S4 object of class \code{\linkS4class{NamedEntityTagger}}.
}

\arguments{
\item{model}{A \code{character} string specifying the trained NER
model the tagger should use.  Available models include a 3 class
(PERSON, ORGANIZATION, LOCATION) model trained on data from CoNLL,
MUC6, MUC7, and ACE (\code{"3-class"}), and a 4 class (PERSON,
ORGANIZATION, LOCATION, MISC) model trained on CoNLL 2003 shared task
training data (\code{"4-class"}).  Both models were trained on
English-language news wires.  }
\item{distsim}{Each trained classifier is available in two forms; one
can load versions of the models that make use of distributional
similarity lexicons by setting \code{distsim} to \code{TRUE}.  These
classifiers perform better than the basic versions but require
substantially more memory (~400MB).}
}

\details{
These objects provide an interface to the Stanford Named Entity
Recognizer, developed by the Stanford Natural Language
Processing Group.  
}

\examples{
  data(crude)
  pcrude<-RSNLCorpus(crude)

  # Tag a corpus with 3-class named entity tags
  pcrude.pos <- tag(pcrude, NamedEntityTagger())
}



\references{
\item{}{Jenny Rose Finkel, Trond Grenager, and Christopher Manning.
2005. Incorporating Non-local Information into Information Extraction
Systems by Gibbs Sampling. Proceedings of the 43nd Annual Meeting of
the Association for Computational Linguistics (ACL 2005), pp. 363-370.
http://nlp.stanford.edu/~manning/papers/gibbscrf3.pdf}
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{NamedEntityTagger}}
}
\keyword{methods}
