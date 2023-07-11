\name{NamedEntityTagger-class}
\docType{class}
\alias{NamedEntityTagger-class}
\alias{model,NamedEntityTagger-method}
\alias{show,NamedEntityTagger-method}
\alias{tag,character,NamedEntityTagger,missing,missing-method}
\alias{tag,character,NamedEntityTagger,PTBTokenizer,missing-method}

\title{Class "NamedEntityTagger"}


\description{Assigns named entity tags (such as person, organization,
or place) to tokens using linear chain Conditional Random Field
sequence models.}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{NamedEntityTagger(model="3-class", distsim=FALSE)} where
\code{model} is the type of model to fit and \code{distsim} indicates
whether or not to load a model that uses a distributional
similarity lexicon to improve performance (see the
\code{\link{NamedEntityTagger}} constructor documentation for details).
These objects provide an interface to the Stanford Named Entity
Recognizer, developed by the Stanford Natural Language
Processing Group.
}

\section{Extends}{
Class \code{"\linkS4class{Tagger}"}, directly.
}

\section{Methods}{
  \describe{
    
    \item{model}{\code{signature(object = "NamedEntityTagger")}:
      Returns the name of trained NER model used by the object.}
    
	 }
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
  \code{\link{tag}}
  \code{\linkS4class{Tagger}}
  \code{\link{NamedEntityTagger}}
}


\keyword{classes}
